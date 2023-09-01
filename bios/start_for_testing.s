;; MAIN.S

;; Currently:
;; Initialization and interrupt shims primarily for
;; testing things.

.section .text.startup
.globl _start
_start:
    ; goals for testing:
    ; 1) switch to DWAS
    ; 2) set up a stack
    ; 3) set up an interrupt handler (vectors in .bss)
    mov     %rh0, 2         ; DWAS
    mov     %address_mode, %rh0     ; enable DWAS
    mov     %spd, 0xA0000000 ; set up a stack
    mov     %rd0, interrupt_handler
    mov     %int_pc, %rd0   ; set up interrupt handler
    ; we have to also set up any interrupt vectors
    ; atm that's just the external one.
    movd    [internal_interrupt_table+32], handle_external_interrupt
    mov     %rx0, 0x0100    ; external interrupt mask
    mov     %int_mask, %rx0 ; enable external interrupts

    ljmp    main
    ; don't expect main to return, but just in case,
    ; make sure the system crashes:
    hlt

;; Simple `puts' function for test cases to use to print messages
;; when they finish. Pass a pointer to a null-terminated string in rd0.
.globl TTY
.set   TTY, 3
.text
puts_putchar:
    storeh  %rh1, TTY
.globl puts
puts:
    mov     %rh1, [%rd0++] ; char c = *msg++
    cmp     %rh1, 0        ; c == '\0'?
    jne     puts_putchar   ; if not, keep going
    ret                    ; if yes, return.

.bss
.type internal_interrupt_table,@object
.p2align 2
internal_interrupt_table:
    .space 4*9

.globl external_interrupt_table
.type external_interrupt_table,@object
.p2align 2
external_interrupt_table:
    .space 4*3

.p2align 3
register_space:
    .space 8*3

.text
.type interrupt_handler,@function
interrupt_handler:
    ; save r0, r1, and ln in designated places, then
    ; index into the table to get a vector.
    mov     [register_space+16], %rq7
    mov     %rd7, register_space+8
    mov     [%rd7], %rq1
    mov     [--%rd7], %rq0

    mov     %rh0, %int_cause    ; why did we interrupt?
    ; the cause is the index into the internal table
    mov     %rd0, [4*%rd0 + internal_interrupt_table] ; rd0 = int vector
    jmp     %rd0

.type handle_external_interrupt,@function
handle_external_interrupt:
    ; index into external table, **call** the vector, then end interrupt.
.set INT_CTL_MMIO, 9 ; we have the same .set in disk.s, maybe make mmio.s?
    loadh   %rh7, INT_CTL_MMIO ; which device triggered this IC interrupt?
                               ; will be 0, 1, or 2.
    mov     %rd7, [4*%rd7 + external_interrupt_table] ; rd7 = extint vector
    call    %rd7
    ; fallthrough to cleanup_interrupt

cleanup_interrupt:
    mov     %rd7, register_space
    mov     %rq0, [%rd7++]
    mov     %rq1, [%rd7++]
    mov     %rq7, [%rd7]
    eret

