;; DISK_TEST_MAIN.S

;; This file provides a `main(void)' function for testing the functions in disk.s.
;; Link with start_for_testing to produce a complete Grape1 ROM.

; we don't return, so don't need to preserve %ln
.text
.globl main
.type main,@function
main:
    ; set up the external interrupt table's DISK_DEV vector:
    ; DISK_DEV_NUM is 1 so we want table + 4.
    movsd     [external_interrupt_table + 4], handle_seek_interrupt

    ; that's all the setup we need for now. Initialize the disk:
    mov       %rh0, 0         ; init drive 0
    call      disk_initialize
    ; now write the first sector with our test message...
    mov       %rh0, 0         ; write to drive 0
    mov       %rd1, test_message ; buffer with data to write
    mov       %rd2, 0         ; write to sector 0
    movz      %rd8, 1         ; write one sector
    call      disk_write

    cmph      %rh0, RES_OK    ; did that write succeed?
    je        test_success    ; print success message
test_fail:
    mov       %rd0, fail_message
    call      puts
    hlt
test_success:
    mov       %rd0, success_message
    call      puts
    hlt

.section .rodata
success_message: .asciz "write succeeded!"
fail_message:    .asciz "write failed :("

.p2align 3
test_message:    .ascii "Hello to the world of (simulated) hard drives!"

