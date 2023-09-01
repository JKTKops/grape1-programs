;;  EHCO.S
;;  A small echo-kernel for the Grape1 (and compatible) devices
;;  which simply acknowledges and handles keyboard interrupts.
;;  All pending characters from the keyboard are drained.
;;  Printable characters are drained to the tty, non-printable
;;  characters are ignored.
;;
;;  Key release events are ignored. Holding a key doesn't
;;  cause repetition.

    ; external interrupt cause is 8
    .set EXTERNAL_INT, 8
    ; MMIO address for write-only tty is 3
    .set TTY, 3
    ; MMIO address for key info is 6. Reading it is destructive!
    .set WHICH_KEY, 6
    ; MMIO address for key event pending is 7
    .set KEY_EVENT, 7
    ; MMIO address 9 is for reading the device ID
    ; and acknowledging interrupts from the given ID.
    .set INT_DEV, 9
    .set INT_ACK, 9
    ; Keyboard is Dev #0 for interrupts
    .set KEY_DEV, 0

    .text
    .globl _start
_start:
    movx    %r0, echo_handler
    movx    %int_pc, %r0
    movx    %r0, 0x100 ; mask for external interrupt bit
    movx    %int_mask, %r0 ; enable external interrupts
echo_loop:
    wait
    jmp     echo_loop

;; Jump here to crash the little echo system.
    .text
echo_crash:
    movx    %sp, crash_msg  ; use sp so that we can pop letters
    movx    %r0, %sp        ; copy to make endpoint
    addx    %r0, crash_msg_len ; char *msg_end = msg_p + msg_len
echo_crash_loop:
    poph    %r1             ; r1 = *msg_p++
    storeh  %r1, TTY        ; putchar(r1)
    cmpx    %sp, %r0        ; msg_p < msg_end
    jb      echo_crash_loop
    hlt

;    .data  normally I'd want this in .data but the script kinda implodes.
crash_msg:  .ascii "echo.s aborted!"
    .set    crash_msg_len, . - crash_msg

    .text
echo_handler:
    ;; we own all the registers here, so we don't need to save them.
    movh    %r0, %int_cause
    cmph    %r0, EXTERNAL_INT   ; is cause external?
    jne     echo_crash          ; if it's not, crash
    loadh   %r0, INT_DEV        ; which device interrupted us?
    cmph    %r0, KEY_DEV        ; was it the keyboard?
    jne     echo_crash          ; otherwise, crash.
    ; ok, so we've been interrupted by the keyboard.
read_keys:
    ; on the first entry here, there's definitely a character since
    ; we were in fact inerrupted. So check before looping instead
    ; of at the top.
    ; Take care - we can only read WHICH_KEY once. The keyboard
    ; works like a queue, and reading this pops it. No way around it.
    loadx   %r0, WHICH_KEY      ; Which key is pressed? {up`1, key`8}
    ; If the keypress wasn't down, ignore it. If it wasn't printable,
    ; that is [32,127], then also ignore it.
    ; Otherwise, ship it to the tty.
    testx   %r0, 0x100          ; was the keypress a release?
    jnz     check_more_keys     ; if yes, ignore it.
    ; otherwise, up is 0 and the value is just 'key'.
    cmph    %r0, 32             ; below least printable?
    jb      check_more_keys
    cmph    %r0, 127            ; above greatest printable?
    ja      check_more_keys
    ; otherwise it's printable. Print it!
    storeh  %r0, TTY
check_more_keys:
    ; are there more key events waiting for us?
    loadh   %r0, KEY_EVENT      ; if 1, there's another pending event.
    cmph    %r0, 1
    je      read_keys           ; in which case, go read more keys.

    ; otherwise, we've handled all pending keyboard interrupts.
    ; Acknowledge the interrupt to the IC unit and bail.
echo_cleanup:
    movh    %r0, KEY_DEV        ; acknowledging an interrupt to KEY_DEV.
    storeh  %r0, INT_ACK        ; acknowledge it.
    eret                        ; and we're done

