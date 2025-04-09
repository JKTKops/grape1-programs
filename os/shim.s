; Entry point shim for loader.
;
; Currently, assumes data at file input is a well-formed ELF file,
; reads the appropriate data for the section header table to
; compute the size of the file, then copies the file.
; Artificial 64kb limit.

.set FILE, 0    ; 8B file i/o

    .bss
elf_file:
    .space 65536

    .text
    .globl _start
_start:
    ; s0: pointer into elf_file

    mov   %rh0, 2
    mov   %address_mode, %rh0
    mov   %rd0, faulthandler
    mov   %int_pc, %rd0
    mov   %spd, 0xA0000000

    ; read the first 56 bytes of the file
    ;   a1: iteration count
    ;   a2: temporary data xf
    mov   %ah1, 7
    mov   %sd0, elf_file
1:  loadq %aq2, FILE            ; 8 bytes of file -> a2
    mov   [%sd0], %aq2          ; *p = a2
    add   %sd0, 8               ; p += 8
    sub   %ah1, 1               ; --i
    jnz   1b                    ; not zero yet? repeat

    ; first 56 bytes loaded
    ; offset 32 contains the section header offset
    ; which as far as we're concerned is the end of the file.
    ; This will include the symbol table which we don't really need,
    ; but it's more difficult to exclude it and we can always
    ; `strip -R` our binaries to get rid of it.

    mov   %ad0, [elf_file + 32] ; a0 <- shoff = total # bytes to read
    sub   %ad0, 56              ; loaded 56 already

    ; load bytes 8 at a time until a0 becomes nonpositive
1:  loadq %aq2,   FILE          ; a2 <- 8 bytes of file
    mov   [%sd0], %aq2          ; *p = a2
    add   %sd0,   8             ; p += 8
    sub   %ad0,   8             ; count -= 8
    jg    1b                    ; loop until a0 is <= 0

    ; file is loaded

    mov   %ad0, elf_file
    call  load_elf
    hlt

    .section .text.faulthandler
faulthandler:
    ; if the cause is exactly 0, we can handle it (syscall),
    ; otherwise wait forever.
    mov   %ax1, %int_cause
    cmp   %ax1, 0
    je    sys
    wait

sys:
    movd  %int_ret_pc, %ad0       ; set transfer address
    movd  %ad0, 0
    movd  %int_ret_priv, %ad0     ; set jump-to-unprivileged

    ; Object program stack will be at the top of lower memory.
    ; (If it was linked with grape1os.x, which it should have been,
    ; then this is only problematic if the memsz of the image is
    ; so close to 2MB that there isn't room for any dynamic stuff.)
    ; zero out all the registers so that we don't accidentally leak
    ; pointers into the "kernel."
    movq  %rq0, 0
    movq  %rq1, 0
    movq  %rq2, 0
    movq  %rq3, 0
    movq  %rq4, 0
    movq  %rq5, 0
    movq  %rq6, 0x20000000
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
