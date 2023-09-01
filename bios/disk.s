;; DISK.S
;; Routines for direct control of the hard disk at the level of sectors.
;; Provides the function 'handle_seek_interrupt' which records that a seek
;; has completed. The function obeys the usual calling convention for
;; interrupt handlers (%r0, %r1, and %ln are volatile, anything else must be saved)
;; and must be invoked at some point while handling seek interrupts
;; (although it's OK if other things happen too).
;; The simplest way to use it is to install
;; it directly in the external interrupt vector. Without it, none of the
;; non-constant routines here will work.
;;
;; A disk seek can be started pre-emptively with 'disk_seek'. A later dwrite
;; or dread call will know that this seek is pending (or completed) and
;; will not cause a separate interrupt; you should try to call 'disk_seek'
;; as soon as possible to hide some latency of the disk seek.
;; Furthermore, disk_write iterates forwards over the sector while disk_read
;; iterates _backwards_ over the sector. Therefore, if you have a choice,
;; you can avoid some seek interrupts for adjacent sectors by matching the
;; direction. You should, however, still call disk_seek, because in the future
;; it will record some statistics about such events.

;; The only failure cases of disk_read and disk_write are if you fail to
;; initialize the drive first, or if the given LBA_t is invalid.

;; When using this module, it must have control of the disk hardware.
;; If you write to the disk MMIO controls yourself, you WILL desynchronize
;; the harddrive state with what this module expects and behavior becomes
;; undefined. Issuing a disk_seek *should* correct the internal state,
;; but only re-initializing the drive can guarantee it.

;;; TYPES

;; An LBA, or Logical Block Address, addresses the blocks (better known as
;; "sectors") on a disk. The Turing Complete disk has addressable units
;; of 8 bytes, however this file hides this and portrays the minimum read/write
;; unit as being NUM_SECTOR_BYTES (a local value configured below).
;; LBA_t is dword for future-proofing; at the moment a byte would suffice.
; typedef DWORD LBA_t

;; A Disk Unit Address, in contrast, is used internally as the addresses
;; written to the disk's seek controls.
; typedef QWORD DUA_t

;; The type of flags describing disk status. At the moment, just 'initialized?'.
; typedef enum {
  .globl STA_NOINIT
  .set   STA_NOINIT, 1
; } DSTATUS;

;; The type of return info about the disk from disk_read/write.
;; Matches FatFS info so that we can drop this right in.
; typedef enum {
  .globl RES_OK
  .set   RES_OK, 0
  .globl RES_ERROR
  .set   RES_ERROR, 1
  .globl RES_WRPRT
  .set   RES_WRPRT, 2  ; write-protected; we don't use this.
  .globl RES_NOTRDY
  .set   RES_NOTRDY, 3 ; disk is not ready
  .globl RES_PARERR
  .set   RES_PARERR, 4 ; invalid parameter
; } DRESULT;

;;; CONFIGURATION

; this one must be 8, we have hardcoded quad operations to work
; on disk units basically everywhere.
.set  DISK_UNIT_LSIZE, 3
.set  DISK_UNIT_SIZE, 1 << DISK_UNIT_LSIZE
; This one can be configured, code should work if it's a power
; of two which is at least 1, though I don't recommend going over 256.
.set  LNUM_SECTOR_UNITS, 6
.set  NUM_SECTOR_UNITS, 1 << LNUM_SECTOR_UNITS
.set  NUM_SECTOR_BYTES, DISK_UNIT_SIZE * NUM_SECTOR_UNITS ; 512
.set  LNUM_SECTOR_BYTES, DISK_UNIT_LSIZE + LNUM_SECTOR_UNITS ; 9
; How many sectors are on the disk?
.set  NUM_SECTORS, 1560

;;; MMIO values
; Write a DUA_t to this address to start a seek.
; It must interrupt us when the seek is completed.
.set  DISK_SEEK_MMIO, 0
; Reading from this address obtains the value at the current
; disk DUA_t and decrements the head.
.set  DISK_READ_MMIO, 8
; Writing to this address stores the value at the current
; disk DUA_t and increments the head.
.set  DISK_WRITE_MMIO, 8
; This address communicates with the interrupt controller.
; Reading from it should be unnecessary, but writing to it
; acknowledges an interrupt for the given device ID.
.set  INT_CTL_MMIO, 9
; This device value from the Interrupt Controller indicates
; the disk. It shows that a seek has completed, and writing
; this device ID to the IC acknowledges seek interrupts.
.globl DISK_DEV_NUM
.set   DISK_DEV_NUM, 1

;;; GLOBAL STATE
.bss
; Internally, this is where the head is (in sectors, not units),
; which may not match the last call to disk_seek.
.align 2
.type last_seek_target,    @object
last_seek_target:    .skip 4  ; LBA_t
.type last_seek_completed, @object
; Is 'last_seek_target' actually the location of the head?
last_seek_completed: .skip 1  ; bool
.type disk_status,         @object
disk_status:         .skip 1  ; DSTATUS
.type disk_initialized,    @object
disk_initialized:    .skip 1  ; bool

; several functions need to busy-wait for a seek to complete.
.macro  wait_for_seek
wait_for_seek_\@:
    cmph    [last_seek_completed], 0
    je      wait_for_seek_\@
.endm


.text
.global handle_seek_interrupt
.type handle_seek_interrupt,@function
.p2align 3
handle_seek_interrupt:
    mov     %rh0, DISK_DEV_NUM
    storeh  %rh0, INT_CTL_MMIO       ; acknowledge SEEK interrupt
    movh    [last_seek_completed], 1 ; seek target is valid
    ret

;;; ------ DSTATUS disk_initialize (BYTE pdrv) -------
;;; | Initialize the given physical drive for use.   |
;;; | If the drive is not drive 0, nothing happens.  |
;;; |                                                |
;;; | inputs:                                        |
;;; |   %rh0: Physical drive number. Must be 0.      |
;;; |                                                |
;;; | outputs:                                       |
;;; |   %rh0: Drive status flags after init.         |
;;; --------------------------------------------------
.text
.global disk_initialize
.type disk_initialize,@function
; This function can be called and must re-initialize the drive.
; In that case, we cannot assume that 'last_seek_target' is
; coherent, so we always start a new seek here.
disk_initialize:
    cmp     %rh0, 0           ; initializing drive 0?
    mov     %rh0, STA_NOINIT  ; otherwise it wont be initted
    retne                     ; so return
    ; FIXME: this is actually unsound. The safest thing to do would
    ; be to mark the last_seek_target as an invalid sector (e.g. -1)
    ; and set last_seek_completed to true. This will force a seek next
    ; time one is requested without having to create a dangerous critical
    ; section here.
    mov     %rq0, 0           ; Prepare disk_seek(0)
    storeq  %rq0, DISK_SEEK_MMIO ; do this directly instead of with disk_seek
                                 ; in case the user really wants to re-initizlize
                                 ; the disk. We should probably also disable
                                 ; disk interrupts and clear last_seek_completed
                                 ; but there isn't currently a way to do that.
    lea     %rd1, [disk_initialized]
    mov     %rh2, 1           ; constant 1
    movh    [%rd1], %rh2      ; mark initialized. rd1 - 1 is disk_status.
    mov     %rh0, 0           ; We will eventually return DSTATUS of 0.
    mov     [--%rd1], %rh0    ; store DSTATUS to disk_status.
    ; busy wait until this seek has completed.
    wait_for_seek
    ret

;;; ------------------- DRESULT disk_seek(LBA_t) ----------------------
;;; | PARAM: %rd0 [IN] LBA_t target sector                            |
;;; | RETURNS: result indicating success/failure                                                                |
;;; | Begin a seek to the first unit of the target sector.            |
;;; -------------------------------------------------------------------
.text
.global disk_seek
.type disk_seek,@function
disk_seek:
    ; standard checks: is the disk initialized? Is the target valid?
    cmph    [disk_initialized], 0     ; is disk initialized?
    jz      quick_notrdy              ; if no, RES_NOTRDY
    cmp     %rd0, NUM_SECTORS         ; target < NUM_SECTORS?
    jnb     quick_parerr              ; error if not
disk_seek_nochk:
    cmp     %rd0, [last_seek_target]  ; if (target == last_seek_target) {
    jne     disk_seek_new_tgt
    mov     %rh0, RES_OK              ;   return RES_OK;
    ret
disk_seek_new_tgt:                    ; }
    ; if there's a pending incomplete seek, we have to be extremely
    ; careful not to desync interrupts. This is a rare case, so it's OK to
    ; handle it terribly slowly: wait for the other one to finish.
    wait_for_seek
    mov     [last_seek_target], %rd0  ; last_seek_target = target;
    movh    [last_seek_completed], 0  ; last_seek_completed = 0;
    movz    %rd0, %rd0                ; (uint64_t)target
    storeq  %rq0, DISK_SEEK_MMIO      ; MMIO(DISK_SEEK_MMIO, target);
    movh    %rh0, RES_OK              ; return RES_OK;
    ret

;;; ---------- DRESULT disk_write(BYTE, BYTE*, LBA_t, UINT) -----------
;;; | PARAMS:                                                         |
;;; |   %rh0: [IN] BYTE pdrv                                          |
;;; |     Physical drive number. Must be 0 since we only have one.    |
;;; |   %rd1: [IN] BYTE *buff                                         |
;;; |     The data to write to the disk. Need not be aligned, but     |
;;; |     the write will be significantly faster if it is.            |
;;; |   %rd2: [IN] LBA_t sector                                       |
;;; |     Start sector address to write to.                           |
;;; |   %rd8: [IN] uint32_t count                                     |
;;; |     The number of sectors to write to.                          |
;;; |                                                                 |
;;; | Returns:                                                        |
;;; |   RES_OK       if the write succeeds                            |
;;; |   RES_NOTRDY   if the drive has not been initialized            |
;;; |   RES_PARERR   if an invalid parameter (incl. pdrv != 0)        |
;;; |                                                                 |
;;; | Write bytes from the given byte array to the disk.              |
;;; | The number of bytes to write is NUM_SECTOR_BYTES * count.       |
;;; |                                                                 |
;;; -------------------------------------------------------------------
.text
.global disk_write
.type disk_write,@function
disk_write:
    cmp     %rh0, 0           ; pdrv == 0?
    jne     quick_parerr      ; fast return RES_PARERR
    cmp     %rh0, [disk_initialized] ; !initialized? (that is, initialize==false)
    je      quick_notrdy      ; fast return RES_NOTRDY

    ; temporary enforce buff is qword aligned.
    testd   %rd1, 7           ; aligned only if this is 0
    jnz     quick_parerr      ; give PARERR instead of #MA.

    ; trust that *buff is valid, but verify sector and count
    mov     %rd0, NUM_SECTORS     ; need this twice
    cmp     %rd2, %rd0            ; sector <? NUM_SECTORS
    jnb     quick_parerr          ; if not, RES_PARERR
    add     %rd8, %rd2            ; sector_stop = sector + count
    cmp     %rd8, %rd0            ; sector_stop <=? NUM_SECTORS
    jnbe    quick_parerr          ; if not, parerr
    cmp     %rd2, %rd8            ; sector ==? sector_stop
    je      disk_write_ret_ok     ; if yup, we're already done.

    ; inputs are valid. Seek the sector.
    mov     %rd0, %rd2            ; arg0 = sector
    push    %lnd
    push    %rd1                  ; save buff
    push    %rd2                  ; save sector
    push    %rd8                  ; save sector_stop
    call    disk_seek_nochk
    pop     %rd8                  ; unwind
    pop     %rd2
    pop     %rd1                  ; I haven't fetched %ln, will be scratch

    ; busywait until seek is done (but it's had around 20 cycles already)
    wait_for_seek

    ; OK, we have everything we need. We'll copy 8 units per loop,
    ; and we have to do that NUM_SECTOR_UNITS / 8 times.
    ; per sector.
    ; We're copying forwards here, but for simplicity, we just use
    ; a nested loop structure.
    ; At this point, sector is in %rd2, stop_sector in rd8,
    ; buff is still in rd1, and r0 and ln are available.
disk_write_outer:                 ; for (sector; sector < stop_sector; sector++) {
    mov     %rh0, NUM_SECTOR_UNITS / 8;   count = 64 / 8 [or whatever NUM_SECTOR_UNITS is]
disk_write_inner:                 ;   for (count; count > 0; --count) {
    mov     %lnq, [%rd1++]        ;     t = *buff++;
    storeq  %lnq, DISK_WRITE_MMIO ;     write_unit(t)
    mov     %lnq, [%rd1++]        ;     t = *buff++;
    storeq  %lnq, DISK_WRITE_MMIO ;     write_unit(t)
    mov     %lnq, [%rd1++]        ;     t = *buff++;
    storeq  %lnq, DISK_WRITE_MMIO ;     write_unit(t)
    mov     %lnq, [%rd1++]        ;     t = *buff++;
    storeq  %lnq, DISK_WRITE_MMIO ;     write_unit(t)
    mov     %lnq, [%rd1++]        ;     t = *buff++;
    storeq  %lnq, DISK_WRITE_MMIO ;     write_unit(t)
    mov     %lnq, [%rd1++]        ;     t = *buff++;
    storeq  %lnq, DISK_WRITE_MMIO ;     write_unit(t)
    mov     %lnq, [%rd1++]        ;     t = *buff++;
    storeq  %lnq, DISK_WRITE_MMIO ;     write_unit(t)
    mov     %lnq, [%rd1++]        ;     t = *buff++;
    storeq  %lnq, DISK_WRITE_MMIO ;     write_unit(t)
    subh    %rh0, 1               ;     now count = count - 1
    jnz     disk_write_inner      ;   }
    add     %rd2, 1               ;   now sector = sector + 1
    cmp     %rd2, %rd8            ;   sector <? stop_sector
    jb     disk_write_outer       ; }

    ;; bump forward the seek info.
    mov     [last_seek_target], %rd2 ; last_seek_target = sector
    ;; return RES_OK
    pop     %lnd
disk_write_ret_ok:
    mov     %rh0, RES_OK
    ret

; shared by disk_write and disk_read and disk_seek
quick_parerr:
    mov     %rh0, RES_PARERR
    ret
; ditto.
quick_notrdy:
    mov     %rh0, RES_NOTRDY
    ret

;;; ---------- DRESULT disk_read(BYTE, BYTE*, LBA_t, UINT) ------------
;;; | PARAMS:                                                         |
;;; |   %rh0: [IN] BYTE pdrv                                          |
;;; |     Physical drive number. Must be 0 since we only have one.    |
;;; |   %rd1: [OUT] BYTE *buff                                        |
;;; |     Where to store the data read from disk. The buffer need not |
;;; |     be aligned, however, the read will be singificantly faster  |
;;; |     if it is.                                                   |
;;; |   %rd2: [IN] LBA_t sector                                       |
;;; |     Start sector address to read from.                          |                           
;;; |   %rd8: [IN] uint32_t count                                     |
;;; |     The number of sectors to read from.                         |
;;; |                                                                 |
;;; | Returns:                                                        |
;;; |   RES_OK       if the write succeeds                            |
;;; |   RES_NOTRDY   if the drive has not been initialized            |
;;; |   RES_PARERR   if an invalid parameter (incl. pdrv != 0)        |
;;; |                                                                 |
;;; | Write bytes from the given byte array to the disk.              |
;;; | The number of bytes to write is NUM_SECTOR_BYTES * count.       |
;;; |                                                                 |
;;; -------------------------------------------------------------------

.text
.global disk_read
.type disk_read,@function
disk_read:
    cmp     %rh0, 0           ; pdrv == 0?
    jne     quick_parerr      ; fast return RES_PARERR
    cmp     %rh0, [disk_initialized] ; !initialized? (that is, initialized==false)
    je      quick_notrdy      ; fast return RES_NOTRDY

    ; temporary enforce buff is qword aligned.
    testd   %rd1, 7           ; aligned only if this is 0
    jnz     quick_parerr      ; give PARERR instead of #MA.

    ; trust that *buff is valid, but verify sector and count
    mov     %rd0, NUM_SECTORS     ; need this twice
    cmp     %rd2, %rd0            ; sector <? NUM_SECTORS
    jnb     quick_parerr          ; if not, RES_PARERR
    ; we're going to read backwards, so the first unit is actually
    ; the unit immediately before the first unit of sector + count.
    ; We keep count to use as a counter in the loop.
    add     %rd2, %rd8            ; sector = sector + count
    cmp     %rd2, %rd0            ; sector_stop <=? NUM_SECTORS
    jnbe    quick_parerr          ; if not, parerr
    cmp     %rd8, 0               ; count ==? 0
    je      disk_read_ret_ok      ; if yup, we're already done.

    ; restore sector to set last_seek_target at the end
    sub     %rd2, %rd8            ; sector = sector_stop - count

    ; inputs are valid. Seek the **stop** sector.
    ; We're going to read backwards.
    mov     %rd0, %rd2            ; arg0 = sector_stop
    push    %lnd
    push    %rd1                  ; save buff
    push    %rd2                  ; save sector
    push    %rd8                  ; save count
    call    disk_seek_nochk
    pop     %rd8                  ; unwind
    pop     %rd2
    pop     %rd1                  ; I haven't fetched %ln, will be scratch

    ; Since we're going to read backwards, adjust the buffer to just past
    ; the last word.
    mov     %rd0, %rd8            ; r0 = count
    shl     %rd0, LNUM_SECTOR_BYTES ; r0 *= NUM_SECTOR_BYTES
    add     %rd1, %rd0            ; buff += count * NUM_SECTOR_BYTES

    ; busywait until seek is done (but it's had around 28 cycles already)
    wait_for_seek

    ; OK, we have everything we need. We'll copy 8 units per loop,
    ; and we have to do that NUM_SECTOR_UNITS / 8 times
    ; per sector.
    ; We're copying backwards here, and for simplicity, we just use
    ; a nested loop structure.
    ; At this point, sector is in %rd2, count is in %rd8
    ; buff is still in rd1, and r0 and ln are available.
disk_read_outer:                        ; for ( ; count > 0; --count) {
    mov     %rh0, NUM_SECTOR_UNITS / 8  ;   ucnt = 64 / 8 [or whatever NUM_SECTOR_UNITS is]
disk_read_inner:                        ;   for ( ; ucnt > 0; --ucnt) {
    loadq   %lnq, DISK_READ_MMIO        ;     t = READ_UNIT()
    movq    [--%rd1], %lnq              ;     *--buff = t
    loadq   %lnq, DISK_READ_MMIO        ;     t = READ_UNIT()
    movq    [--%rd1], %lnq              ;     *--buff = t
    loadq   %lnq, DISK_READ_MMIO        ;     t = READ_UNIT()
    movq    [--%rd1], %lnq              ;     *--buff = t
    loadq   %lnq, DISK_READ_MMIO        ;     t = READ_UNIT()
    movq    [--%rd1], %lnq              ;     *--buff = t
    loadq   %lnq, DISK_READ_MMIO        ;     t = READ_UNIT()
    movq    [--%rd1], %lnq              ;     *--buff = t
    loadq   %lnq, DISK_READ_MMIO        ;     t = READ_UNIT()
    movq    [--%rd1], %lnq              ;     *--buff = t
    loadq   %lnq, DISK_READ_MMIO        ;     t = READ_UNIT()
    movq    [--%rd1], %lnq              ;     *--buff = t
    loadq   %lnq, DISK_READ_MMIO        ;     t = READ_UNIT()
    movq    [--%rd1], %lnq              ;     *--buff = t
    subh    %rh0, 1                     ;     now --ucnt
    jnz     disk_read_inner             ;   }
    subd    %rd8, 1                     ;   now --count
    jg      disk_read_outer             ; } // loop if count was > 1, i.e. now > 0.

    ;; bump back seek_target info
    mov     [last_seek_target], %rd2    ; last_seek_target = sector
    ;; return RES_OK
    pop     %lnd
disk_read_ret_ok:
    mov     %rh0, RES_OK
    ret
