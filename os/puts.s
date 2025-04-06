    .set TTY, 3

    .text
; ---------------------------------------------------
; | void puts(const char *s)                        |
; |                                                 |
; | Output the given string to the TTY device.      |
; | Inputs:                                         |
; |   s: A null-terminated C string.                |
; | Outputs: (void)                                 |
; ---------------------------------------------------
puts_char:
    storeh  %ah1, TTY
    .globl puts
puts:
    mov     %ah1, [%ad0++]  ; char c = *msg++
    cmp     %ah1, 0         ; c == '\0' ?
    jne     puts_char       ; if not, keep going.
    ret                     ; if yes, return.
