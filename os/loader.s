;;; ELF Loader
;;; Call load_elf() with a pointer to ELF data.
;;; This data should reside in upper RAM, and should denote a
;;; well-formed ELF executable which places all of its segments
;;; which occupy memory in lower RAM (0x00000020..UPPER_LIMIT).
;;; UPPER_LIMIT is configured below. For Grape1, it is 0x001FFFFF.
;;;
;;; A process image is created from the file, and then entered.
;;; If any part of this process fails, a message will be printed.
;;; Such messages look like:
;;;   Could not execute binary file. Reason: [...]
;;;
;;; All memory segments are silently promoted to RWX segments.
;;;
;;; This loader is **not** a kernel, though it is intended that
;;; a derivative of this loader eventually be included in one.
;;; As such, it does not store any metadata about the process image
;;; anywhere and does not initialize registers like %sp, so the
;;; executable being loaded is responsible for initializing its
;;; own stack. Additionally, no _start symbol is defined in this file.

;;; BEGIN IMPLEMENTATION.

.include "elf_hdr.s"

; load_elf will return 126 (cannot execute) if it fails for any reason.
; When failure is detected, an error message should be printed.

    .text
print_err_header:
    lea   %ad0, [err_header]
    ljmp  puts                ; tail call
    .section .rodata
err_header: .asciz "Could not execute binary file. Reason: "

    .text
; void unknown_format()
;   Print the message for unknown format.
unknown_format:
; stack layout:
;   0: ra
    push  %lnq
    call  print_err_header
    lea   %ad0, [malformed_msg]
    pop   %lnq
    ljmp  puts  ; tail call
    .section .rodata
malformed_msg:  .asciz "format error\n"


; As we read the header, we will need to know the offset of each field.
; struct ELF_header {
;   int   MAGIC;    0
;   char  CLASS;    4
;   char  DATA;     5
;   char  VERSION;  6
;   char  OSABI;    7
;   long  PAD;      8
;   short TYPE;     16
;   short MACH;     18
;   int   VERSION2; 20
;   int   ENTRY;    24
;   int   PHOFF;    28
;   int   SHOFF;    32
;   int   FLAGS;    36 (but ignored)
;   short EHSIZE;   40
;   short PHENTSZ;  42
;   short PHNUM;    44
;   short SHENTSZ;  46
;   short SHNUM;    48
;   short SHSTRNDX; 50 (but ignored)
;   void  SIZE[0];  52
; }
.set OFMAGIC, 0  ; value must be 0x464c457f
.set OFCLASS, 4  ; value must be 1
.set OFDATA,  5  ; value must be 1
.set OFVERS,  6  ; value must be 1
.set OFOSABI, 7  ; value must be 0
.set OFPAD,   8  ; value is ignored
.set OFTYPE,  16 ; value must be 2 (ET_EXEC)
.set OFMACH,  18 ; value must be 0xe7ca
.set OFVERS2, 20 ; value must still be 1. Why are there two of these?
.set OFENTRY, 24
.set OFPHOFF, 28 ; checked to contain 0x34, but value is copied (future proofing)
.set OFSHOFF, 32 ;
.set OFEHSZ,  40 ; must contain 52.
.set OFPHENTSZ, 42 ; must contain 32.
.set OFPHNUM, 44
.set OFSHENTSZ, 46 ; must contain 40.
.set OFSHNUM, 48
.set EHSIZE,  52 ; size of header.

; ---------------------------------------------------
; | int read_header(struct ELF_header *eh,          |
; |                 struct ELF_header_data  *dat)   |
; |                                                 |
; | Read the given ELF header and validate its      |
; | fields. Return 126 and print message if it's    |
; | malformed.                                      |
; | Inputs:                                         |
; |   eh: pointer to ELF header data.               |
; |   dat: addr to which we should write data.      |
; | Outputs:                                        |
; |   0 is returned on success, 126 on failure.     |
; |   Additionally, info parsed from the header is  |
; |   written to the ELF_header_data given.         |
; |   Nothing is written on failure.                |
; ---------------------------------------------------

    .text
read_header:
; stack layout:
;   We don't make a frame until we're failing.
    .macro rh_assert size exp
    cmp\size   %a2, \exp
    jne   read_header_fail
    .endm
    mov   %ad2, [%ad0]            ; int magic = eh->MAGIC;
    rh_assert d 0x464c457f
    mov   %ah2, [%ad0 + OFCLASS]  ; char class = eh->CLASS;
    rh_assert h 1
    mov   %ah2, [%ad0 + OFDATA]   ; char data = eh->DATA;
    rh_assert h 1
    mov   %ah2, [%ad0 + OFVERS]   ; char version1 = eh->VERSION;
    rh_assert h 1
    mov   %ah2, [%ad0 + OFOSABI]  ; char osabi = eh->OSABI;
    rh_assert h 0
    mov   %ax2, [%ad0 + OFTYPE]   ; short type = eh->TYPE;
    rh_assert x 2
    mov   %ax2, [%ad0 + OFMACH]   ; short mach = eh->MACH;
    rh_assert x 0xe7ca
    mov   %ad2, [%ad0 + OFVERS2]  ; int version2 = eh->VERSION2;
    rh_assert d 1
    mov   %ad2, [%ad0 + OFPHOFF]  ; void *phoff = eh->PHOFF;
    rh_assert d 0x34
    mov   %ax2, [%ad0 + OFEHSZ]   ; short ehsize = eh->EHSIZE;
    rh_assert x 52
    mov   %ax2, [%ad0 + OFPHENTSZ] ; short phentsz = eh->PHENTSZ;
    rh_assert x 0x20
    mov   %ax2, [%ad0 + OFSHENTSZ] ; short shentsz = eh->SHENTSZ;
    rh_assert x 0x28
    
    ; at this point we've successfully read a header. Yay!
    ; We won't try to prevent overflow at this point and we'll
    ; validate ENTRY later, in a more convenient place.
    stehd %ad0, %ad1, raw_data    ; dat->raw_data = eh
    mov   %ad2, [%ad0 + OFENTRY]  ; code *eh_entry = eh->entry
    stehd %ad2, %ad1, entry       ; dat->entry = ad2
    mov   %ad2, [%ad0 + OFPHOFF]  ; read phdr offset (checked earlier)
    addd  %ad2, %ad0              ;   phdr offset + eh is phdr address
    stehd %ad2, %ad1, ph          ; dat->ph = phdr address
    mov   %ad2, [%ad0 + OFSHOFF]  ; read shdr offset
    addd  %ad2, %ad0              ;   shdr offset + eh is shdr address
    stehd %ad2, %ad1, sh          ; dat->sh = shdr address
    mov   %ax2, [%ad0 + OFPHENTSZ] ; read phentsz (checked earlier)
    stehd %ax2, %ad1, phentsz     ; set dat->phentsz
    mov   %ax2, [%ad0 + OFPHNUM]  ; read phnum
    stehd %ax2, %ad1, phnum
    mov   %ax2, [%ad0 + OFSHENTSZ] ; read shentsz (checked earlier)
    stehd %ax2, %ad1, shentsz
    mov   %ax2, [%ad0 + OFSHNUM]
    stehd %ax2, %ad1, shnum

    mov   %ah0, 0                 ; return 0
    ret

    ; now handle failure
read_header_fail:
    push  %lnq
    call  unknown_format
    pop   %lnq
    mov   %ax0, 126               ; return 126
    ret

; ------------------------------------------------
; int validate_segment_mem(int vaddr, int memsz)
;
; Ensure that the given vaddr and memsz describe a
; segment which is well-placed in memory.
; (above 0x20 and below 0x00200000)
; Returns 0 for valid and 126 for invalid.
; If invalid, also prints an error message.
; ------------------------------------------------
    .text
validate_segment_mem:
    sub   %ad0, 0x20              ; vaddr - 0x20 (lower bound)
    cmp   %ad0, 0x00200000-0x20   ; vaddr-0x20 <? 2MB-0x20
    jae   invalid_segment_mem     ; too big: invalid
    addd  %ad0, %ad1              ; vaddr-0x20+memsz
    jc    invalid_segment_mem     ; if that carried: memsz is def too big
    cmp   %ad0, 0x00200000-0x20   ; upper segment limit <? 2MB
    jae   invalid_segment_mem     ; if not: invalid
    mov   %ad0, 0                 ; otherwise: return success
    ret

invalid_segment_mem:
    pushq %lnq
    call  print_err_header
    movd  %ad0, misplaced_segment_msg
    call  puts
    popq  %lnq
    mov   %ad0, 126
    ret

    .section .rodata
misplaced_segment_msg:  .asciz "badly placed segment\n"

; ----------------------------------------------------
; int validate_segment(struct phdr *phent)
;
; Ensure the given phent describes a well-placed
; segment: it is located in-bounds and `align` makes
; sense. We could just trust it, but these checks
; are easy!
; Returns like validate_segment_mem.
; ----------------------------------------------------
    .text
validate_segment:
    mov   %ad2, %ad0              ; a2 <- phent
    ldphd %ad0, %ad2, vaddr       ; a0 <- phent->vaddr
    ldphd %ad1, %ad2, align       ; a1 <- phent->align
    cmp   %ad1, 8                 ; is align at least 8?
    jl    bad_segment_align       ; if not, tail-call error routine

    ; So phent->align is at least 8 which is great, but that just
    ; guarantees that phent->vaddr and phent->offset are cong. mod 8.
    ; We actually want them to be zero mod 8. So check that too.
    movd  %ad1, 7                 ; a1 = 0b111
    testd %ad0, %ad1              ; flags <- phent->vaddr & 0b111
    jnz   bad_segment_align       ; invalid if that's not 0
    ; otherwise tail-call validate_segment_mem
    ldphd %ad1, %ad2, memsz       ; a1 <- phent->memsz
    jmp   validate_segment_mem    ; tail-call

; ----------------------------
; int bad_segment_align()
; Prints a message and returns 126 for insufficiently
; aligned segment.
; ----------------------------
    .text
bad_segment_align:
    pushq %lnq
    call  print_err_header
    movd  %ad0, unaligned_segment_msg
    call  puts
    popq  %lnq
    mov   %ad0, 126
    ret

    .section .rodata
unaligned_segment_msg:  .asciz "insufficiently aligned segment\n"

; ----------------------------------------------------
; int validate_entry(void *entry)
;
; Validate the entry code pointer. For now, we only
; test that it's a pointer into low memory.
; ----------------------------------------------------
    .text
validate_entry:
    cmpd  %ad0, 0                 ; entry negative (high mem)?
    jn    1f                      ; if yes, fail
    movh  %ah0, 0                 ; otherwise return 0
    ret
1:  pushq %lnq
    call  print_err_header
    movd  %ad0, bad_entry_msg
    call  puts
    popq  %lnq
    mov   %ad0, 126
    ret

    .section .rodata
bad_entry_msg:  .asciz "bad entry address\n"

; ----------------------------------------------------
; | int load_segments(struct ELF_header_data *ehd)   |
; |                                                  |
; | Using the (ehd->ph, ehd->phnum) array, load the  |
; | segments described by each header entry,         |
; | validating the segment placement and alignment   |
; | for each. Struct fields are accessed with macros |
; | define in elf_hdr.s.                             |
; ----------------------------------------------------
    .text
load_segments:
    sub   %spd, 40                ; change as needed (keep 8-byte aligned)
    mov   [%spd], %lnq
    mov   [%spd+8], %sq0          ; s0 is ph
    mov   [%spd+16], %sq1         ; s1 is phnum
    mov   [%spd+24], %bpq         ; bp is phentsz
    mov   [%spd+32], %ad0         ; save ehd on the stack

    ldehd %sd0, %ad0, ph          ; s0 = ehd->ph
    ldehd %sx1, %ad0, phnum       ; s1 = ehd->phnum
    ldehd %bpx, %ad0, phentsz     ; bp = ehd->phentsz
      ; phentsz is small (32, as long as this is 32-bit elf) so bpx=bpd
    jmp   ls_loop_decr            ; enter loop at check
      ; this technically allows elf files with phnum = 0
      ; and we really shouldn't load such a file: entry can't
      ; be valid! But for now I think this is OK.

1:  mov   %ad0, %sd0              ; a0 = ph
    call  validate_segment        ; segment is well-placed?
    testd %ad0, %ad0              ; yes if ad0 is 0
    jnz   load_segments_return    ; otherwise return ad0
    mov   %ad0, %sd0              ; a0 = ph
    call  load_segment            ; load ph

    addd  %sd0, %bpd              ; (char*)ph += phentsz (aka ++ph)
    ; now decrement phnum and possibly loop

ls_loop_decr:
    sub   %sx1, 1                 ; --phnum
    jge   1b                      ; loop if phnum >= 0
    mov   %ad0, 0                 ; otherwise return 0

load_segments_return:
    ; no need to restore ehd (in fact, we really don't want to)
    mov   %bpq, [%spd+24]
    mov   %sq1, [%spd+16]
    mov   %sq0, [%spd+8]
    mov   %lnq, [%spd]
    add   %spd, 40
    ret

; ----------------------------------------------------
; | void load_segment(elf *elf, struct phdr *phent)  |
; |                                                  |
; | Load the segment described by *phent into        |
; | memory. f denotes the address of the elf data,   |
; | that is, f + phent->offset is the address of the |
; | segment data loaded from the file image.         |
; | The pointer f must be at least qword aligned!    |
; |                                                  |
; | If the segment is not loadable, returns fast.    |
; |                                                  |
; | This function cannot fail - ensure the phent has |
; | been through validate_segment beforehand, and    |
; | don't try to load a segment that is invalid.     |
; ----------------------------------------------------
    .text
load_segment:
    ldphd %ad2, %ad1, type        ; a2 = phent->type
    cmp   %ad2, 1                 ; type == 1?
    retne                         ; if no, we're done. Segment not loadable.

    ; we need to copy phent->filesz many bytes from (elf + phent->offset)
    ; to phent->vaddr.
    ; Then, if memsz > filesz, we also have to write memsz - filesz zeroes
    ; immediately after the copied data.
    ;
    ; validate_segment has checked not only that phent->align is at least 8,
    ; but also that vaddr (and therefore offset) are so aligned as well.
    ; Therefore we can assume that the inputs to memcpy are 8-byte aligned
    ; pointers. However, filesz may not be a multiple of 8, so the memset
    ; that we use must be prepared to accept any alignment of pointer.
    ; Such restricted memcpy and memset functions are provided below.
    ; 
    ; To include this loader in a proper OS, use the same memcpy and memset
    ; as the OS uses and remove the ones from here.
    ;
    ; stack map:
    ;   0 ra
    ;   8 phent
    pushq %aq1
    pushq %lnq

    mov   %td0, %ad1              ; stash phent away from args
    ldphd %ad1, %td0, offset      ; a1 = phent->offset
    addd  %ad1, %ad0              ; a1 = phent->offset + elf
    ldphd %ad0, %td0, vaddr       ; a0 = phent->vaddr
    ldphd %ad2, %td0, filesz      ; a2 = phent->filesz
    call  memcpy                  ; memcpy(phent->vaddr, phent->offset + f,
                                  ;        phent->filesz)

    mov   %td0, [%spd+8]          ; recover phent
    ldphd %ad0, %td0, vaddr       ; a0 = phent->vaddr
    ldphd %ad1, %td0, filesz      ; a1 = phent->filesz
    addd  %ad0, %ad1              ; a0 = phent->vaddr + phent->filesz
    ldphd %ad2, %td0, memsz       ; a2 = phent->memsz
    subd  %ad2, %ad1              ; a2 = phent->memsz - phent->filesz
    movd  %ad1, 0                 ; a1 = 0
    call  memset                  ; memset(phent->vaddr + phent->filesz, 0,
                                  ;        phent->memsz - phent->filesz)

    popq  %lnq
    addd  %spd, 8
    ret

; -----------------------------------------------------
; | void memcpy(void *dst, void *src, size_t n_bytes) |
; |                                                   |
; | Memcpy. dst and src must be 0 mod 8.              |
; -----------------------------------------------------
    ; FIXME: it would be faster to copy backwards,
    ; because we wouldn't have to mess with n_bytes.
    ; It's more than significant enough to warrant
    ; using the stack or more temp variables to store
    ; a bound pointer. Worth doing.
    .text
memcpy_qw:  ; n_bytes >= 8
    movq  %tq0, [%ad1++]          ; tmp = *src; src += 8
    movq  [%ad0], %tq0            ; *dst = tmp
    addd  %ad0, 8                 ; dst += 8
    subd  %ad2, 8                 ; n_bytes -= 8
memcpy:
    cmpd  %ad2, 8                 ; at least 8 bytes left to move?
    jae   memcpy_qw               ; yes, keep looping

    ; if n_bytes is now 0, we're done:
    cmpd  %ad2, 0
    rete

    ; otherwise, try copy 4 bytes, then 2, then 1.
memcpy_try_dw:
    testd %ad2, 4                 ; n_bytes & 0b100 ?
    jz    memcpy_try_xw           ; no, try 2 bytes
    movd  %td0, [%ad1++]          ; tmp = *src, src += 4
    movd  [%ad0], %td0            ; *dst = tmp
    addd  %ad0, 4                 ; dst += 4
memcpy_try_xw:
    testd %ad2, 2                 ; n_bytes &0b010 ? ...
    jz    memcpy_try_hw
    movx  %tx0, [%ad1++]
    movx  [%ad0], %tx0
    addd  %ad0, 2
memcpy_try_hw:
    testd %ad2, 1
    retz
    movh  %th0, [%ad1]
    movh  [%ad0], %th0
    ret

; -----------------------------------------------------
; | void memset(void *dst, char value, size_t n)      |
; |                                                   |
; | memset, no alignment requirement.                 |
; -----------------------------------------------------
    ; write the data backwards so we can use push
    .text
memset:
    cmpd  %ad2, 0                 ; n is 0?
    rete                          ; fast return if so.
    movh  %th0, %ah1              ; copy value to t0

    addd  %ad0, %ad2              ; dst' = dst + n
    ; now dst' is actually one byte past the end of where
    ; we should write. If the value is currently 8-byte
    ; aligned, we can start writing
    ; qw copies of value. Otherwise we will have to
    ; subtract 1 and write single bytes until it's aligned.
    testd %ad0, 7                 ; 8 byte aligned?
    jz    memset_qwify_value      ; yes, go prepare for qw portion
                                  ; no, start writing single bytes
memset_intro_loop:
    movh  [--%ad0], %ah1          ; *(--dst) = value
    subd  %ad2, 1                 ; n--
    retz                          ; return if n is now 0
    testd %ad0, 7                 ; otherwise test alignment again
    jnz   memset_intro_loop       ; and if not aligned yet, repeat.

memset_qwify_value:
    ; have char value
    ; need char qw_value[8] = {value,value,...}
    ; remember value is copied into t0 already
    ; don't use that version with first instr bc of prefix stall
    shlq  %aq1, 8
    orq   %aq1, %tq0            ; a1 = (value << 8) | value
    movq  %tq0, %aq1            ; t0 = that
    shlq  %aq1, 16              ; now again for 2 bytes
    orq   %aq1, %tq0
    movq  %tq0, %aq1            ; t0 = that
    shlq  %aq1, 32              ; now again for 4 bytes
    orq   %aq1, %tq0

    cmpd  %ad2, 8               ; n >=? 8
    jb    memset_outro          ; If not, skip qw loop (awkward)
    subd  %ad2, 8               ; otherwise, take off 8 now, then
                                ; we stop the qw loop when n is negative.
memset_qw_loop:
    movq  [--%ad0], %aq1        ; *(dest-=8) = qw_value
    subd  %ad2, 8               ; n -= 8
    jae   memset_qw_loop        ; if n is still nonnegative, keep going.
    addd  %ad2, 8               ; add back the 8 that we took off above loop,
                                ; now n is between 0 and 7 inclusive.

memset_outro:
    ; handle any unaligned bytes to write at the start of the region.
    cmpd  %ad2, 0               ; is n == 0?
    rete                        ; if yes, we're done. Return.
memset_outro_loop:
    movh  [--%ad0], %ah1        ; write one byte (low byte of qw_value is value)
    subd  %ad2, 1               ; n--
    jnz   memset_outro_loop     ; if n not yet zero, keep writing single bytes
    ret                         ; when n is zero, we're done.

; ----------------------------------------------------
; | int load_elf(elf *elf)                           |
; |                                                  |
; | Load the given ELF data and transfer control     |
; | to it.                                           |
; | If successful, it will not return.               |
; | On failure, returns 126.                         |
; ----------------------------------------------------
    .text
    .globl load_elf
load_elf:
; stack map:
;   0 ra
;   8 struct ELF_header_data ehd (24 bytes, alignup to 8 = 24)

    sub   %spd, 32                ; change as needed
    mov   [%spd], %lnq

    lea   %ad1, [%spd + 8]        ; &ehd
    call  read_header             ; read_header(elf, &ehd)
    testd %ad0, %ad0              ; check for errors
    retnz                         ; return if errors

    lea   %ad0, [%spd + 8]        ; &ehd
    call  validate_entry          ; validate_entry(&ehd) confirm entry reasonbl
    testd %ad0, %ad0              ; check for errors
    retnz                         ; return if errors

    lea   %ad0, [%spd + 8]        ; &ehd
    call  load_segments           ; load_segments(&ehd)
    testd %ad0, %ad0              ; check for errors
    retnz                         ; return if errors
    ;; note that if there was an error there, we've just left whatever
    ;; fraction of the process image we were able to load.
    ;; Fine for MVP -- but maybe not for future re-use in kernel!

    ;; Another note for kernel: transferring control in this way
    ;; does not work in a complete OS. Getting interrupted after
    ;; setting int_* but before eret would be catastrophic.
    ;; In an OS without virtual memory, most likely whatever requested
    ;; this load has been moved to disk and the process image we are
    ;; creating is intending to run immediately. But either way, we need
    ;; to initialize its PCB, not its registers, and then use some code
    ;; that transfers control to a swapped-in PCB (probably also used
    ;; by the scheduler). Most likely, that code needs to temporarily
    ;; disable interrupts so that we can do this transfer atomically.
    movd  %lnd, 0
    movd  %int_ret_priv, %lnd     ; set jump-to-unprivileged
    movd  %lnd, [%spd + 8 + __EHD_entry] ; %ln = ehd.entry;
    movd  %int_ret_pc, %lnd       ; set jump-to-unprivileged address

    ; can't initialize a stack for the object program, because we don't know
    ; where it wants its stack to be. But we can very well zero out all the
    ; registers so that we don't accidentally leak pointers into the
    ; "kernel."
    movq  %rq0, 0
    movq  %rq1, 0
    movq  %rq2, 0
    movq  %rq3, 0
    movq  %rq4, 0
    movq  %rq5, 0
    movq  %rq6, 0
    movq  %rq7, 0
    movq  %rq8, 0
    movq  %rq9, 0
    movq  %rq10, 0
    movq  %rq11, 0
    movq  %rq12, 0
    movq  %rq13, 0
    movq  %rq14, 0
    movq  %rq15, 0
    eret                          ; Transfer control to proc image in user mode.
