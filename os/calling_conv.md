# Calling Convention for Grape1 OS

This is a 32-bit OS. That means only DWAS is supported. However, the QWORD extension is also supported.
The calling convention considers that data may be 8 bytes even though _pointers_ can only be 4.

Pointers shall be canonical at all times.

## Registers (short tabular form)

| Numeric name | ABI name | purpose | preserved? |
|:------------:|:--------:|---------|------------|
| r0 | a0 | arg/ret | no |
| r1 | a1 | arg/ret | no |
| r2 | a2 | arg | no |
| r3 | s0 | scratch | yes |
| r4 | s1 | scratch | yes |
| r5 | bp | frame ptr | yes |
| r6 | sp | stack ptr | yes |
| r7 | ln | link | no |
| r8 | t0 | temp | no |
| r9 | t1 | temp | no |
| r10 | t2 | temp | no |
| r11 | t3 | temp | no |
| r12 | t4 | temp | no |
| r13 | s2 | scratch | yes |
| r14 | s3 | scratch | yes |
| r15 | s4 | scratch | yes |

Rationale: temp registers are more useful than scratch registers, and argument registers are effectively extra temporaries in many contexts. This gives us an 8-to-5 balance; 8-to-6 when bp is used as an extra scratch register.

The base pointer must preserved to do its job. The stack pointer technically need not be, as the base pointer
could be used to allow the called function to clean up the stack space allocated for its arguments.
This is not desirable, because that space may be arranged on the caller's stack in advance to be re-used.
Letting the caller manage it usually saves instructions; it will be allocated once in the prologue and deallocated
once in the epilogue, rather than being allocated before each `call` instruction and deallocated when that returns.

The link register is by definition not preserved by function calls, and can therefore be used as a temporary
register between calls.

## Stack Management

The stack pointer shall always be 8-byte aligned.

## Passing Arguments

The first three qword-size or smaller arguments shall be passed in `a0`, `a1`, and `a2`.
Further arguments shall be pushed on the stack in reverse order, such that `[%spd]` contains the third argument.
Arguments passed on the stack may be modified by the called function.

By-value structs larger than one qword should not be used. When they must be used, they are passed on the stack.

Optionally, functions may push their first three arguments on the stack in their prologue.
Variadic functions are expected to do this.

## Base Pointer

Then optionally, functions push `bpq` and set `bpd` to the location that it was saved.
This links stack frames and allows for iterating over them, but is not required.

## Returning

Values are returned in `a0`. Structs with two fields, both qword size or smaller,
shall be returned in the `a0|a1` pair, with the first member in `a0`.
Returning other structs by value is not supported. 

C ABI: To implement other structs being returned by value, the caller shall allocate
space for the result, and shall pass a pointer to that space as the first argument.

## System Calls

TODO.
