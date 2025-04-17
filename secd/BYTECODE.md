# Bytecode

A bytecode program is a program header followed by a
sequence of (untyped) bytes. Most of these bytes are part of a code sequence,
but some may be parts of static objects. The interpretation of bytes is
determined solely by what is expected by the program's execution.

Bytes in a code sequence are either _bytecodes_, which are analogous to
_opcodes_ in assembly languages, or they are arguments to a bytecode.
There are TODO bytecodes, given in the following sections.

## Machine State Representation

Some bytecode operations are given formally as a state transition.
Such transitions are operational semantic transitions on a state of the
form `<S, E, C, D>`. `S` is a stack of objects, written as
`x : y : ...`. `E` is an environment pointer, and points to the closure
of the currently-executing function. `C` is a bytecode offset.
When a bytecode begins executing, `C` is always the offset of the bytecode
itself. `D` is a stack of `(E,C,S)` triples forming a return stack.
Transitions may refer to a subset of the machine state such as
`<S> -> <nil:S>`, which is shorthand for `<S, E, C, D> -> <nil:S, E, C, D>`.
When a transition does not specify what happens to `C`, it is implied that
it is updated to point at the following bytecode.

In the current implementation, the S and D stacks are separate.
They both exist in the same region and grow towards eachother.
Stack overflows are not detected.

When a function is entered, the current stack pointer is saved on the `D` stack,
adjusted to point below the function's arguments (see `FASTAP` for example).
Offsets from this saved stack pointer therefore access the function's arguments
and the things pushed over them.
The value `locals[0]` refers to the rightmost argument (which is the lowest
stack object above the saved pointer). `locals[1]` refers to the next
argument from the right, etc. If the function has arity `A`,
then `locals[A]` is the first value pushed after the function began executing.
The behavior of saving stack pointers and the use of `locals` may change in
future versions. The current version enables fast-as-possible implementations
of some combinators by simply manipulating the stack they are given and
rapidly tail-calling something else, but requires the two stacks to be
separate and prevents the use of a register to hold the top stack item
(because it might be accessed via `LLCL` and therefore must be on the stack).
In a future version, this all might change, slowing down some combinators
in exchange for some implementation complexity and (hopefully) performance.

Note that in the original presentation of the SECD machine, operations for
application clear the `S` stack and save it on `D`. We preserve the `S`
stack to be closer to the implementation, which manipulates a stack
pointer.

## Common Abbreviations

Several abbreviations are used throughout, usually for arguments to various
bytecodes.

* `BO` is a 24-bit bytecode offset, used for referring to static objects
  placed directly in the bytecode stream.
* `CP` is a 24-bit bytecode offset, usually used as a jump target.
* `RO` is a 16-bit signed relative offset from the current bytecode.
* `H` is a one-byte number.
* `W` is a two-byte number.
* `D` is a four-byte number.
* `_` is used when the type of the abbreviated value is clear and it is unused.

## Object Representation

The value `nil` is the all-bits-zero value. All other objects are
pointers, but we will simply write the representation of the object pointed to.
For integer, boolean, character, and string values,
we will write `I`, `B`, `C`, or `STR` for arbitrary values respectively.
(Note that `C` therefore refers to the code pointer or a character value,
depending on context.)
For literal values we will simply write the value.

`FUN(CP,N,...)` is a closure object with code pointer CP, arity A,
and the remaining listed values are captured variables.

`PAP(f,...)` is a partial application object for closure `f`.
The remaining listed values are arguments.

`OBJ(D,...)` is an arbitrary object with tag `D` and remaining fields `...`.

# Format of a Bytecode Program

A bytecode program consists of 3 segments: the header,
relocatable static data, and text.

The relocatable static data and text segments can contain data objects
in the format described in RUNTIME.md. These objects **must** be 4-byte
aligned.

## Header

The bytecode header is 3 words, in order: the length of the bytecode
(including the header) **in bytes**, the length of the relocatable static
data segment **in words**, and the offset at which execution should begin.
(Presumably, in the text segment.)

## Relocatable Static Data

After the header, there is space for relocatable static data.
Relocatable static data may contain pointers to other relocatable static
data, in the form of a bytecode offset. (Like all bytecode offsets, it
is an offset from the start of the file.)
Relocatable static objects must be placed end-to-end such that the offset
of an object plus the size in its header is the offset of the next
relocatable static object.

When the bytecode program is loaded, all objects in the relocatable static
data segment are "relocated," meaning all bytecode offsets within the
objects are updated to proper pointers.
There are internal common, non-gc'd objects for the integers in the range
[-1,9] and for both booleans. During relocation, any read offsets under
`Field Value` in the following table be replaced by a pointer to
to the corresponding internal common object.
(Note that the most significant byte of each is `0xFF`, which cannot
be confused for a real bytecode offset due to the 24-bit limit.)

| Field Value | Points to this object |
|-------------|-----------------------|
| `0xFF000000` | `BOOL(false)` |
| `0xFF000001` | `BOOL(true)` |
| `0xFF000002` | `INT(-1)` |
| `0xFF000003` | `INT(0)` |
| `0xFF000004` | `INT(1)` |
| `0xFF000005` | `INT(2)` |
| `0xFF000006` | `INT(3)` |
| `0xFF000007` | `INT(4)` |
| `0xFF000008` | `INT(5)` |
| `0xFF000009` | `INT(6)` |
| `0xFF00000A` | `INT(7)` |
| `0xFF00000B` | `INT(8)` |
| `0xFF00000C` | `INT(9)` |

Static `INT`, `BOOL`, `CHAR`, `STRING`, and `FUN` objects do not have their
fields relocated. For the first four, their fields are immediate and relocation
would destroy them. For `FUN` objects, there should be no reason for a static
closure to have any captured variables; since static closures are immutable,
any captures of a static closure are statically known and should be accessed as
such.

After relocation, static data
objects are indistinguishable from heap objects from the perspective of
the program (although they can be accessed with `LDGW` and `LDGD` bytecodes).
From the perspective of the runtime system, they will never be garbage
collected because they are not heap-allocated.
Note that static data can also exist in the text segment, but static data
in the text segment will not be relocated.

It is important to **set the static bit** in the headers of static data,
**including static data in the text segment,**
else garbage collections may cause them to be permanently lost.
Additionally, static objects are immutable. Attempting to set a field
of a static object to something heap-allocated would be problematic,
because the static objects are not roots for garbage collection.

## Text

The remainder of the program is the text segment. This segment contains
bytecodes and possibly static data that does not require relocations.
Following are descriptions of each of the bytecodes.

Take care that static data placed in the text segment must be 4-byte aligned.

## Meta Bytecodes

These bytecodes affect the operation of the interpreter in some way,
or are otherwise miscellaneous.

| Byte | Name | Operation |
|------|------|-----------|
| `0x00` | `HLT` | Halts the program. If the top value on the stack is an `INT` object, its value is the exit code. Otherwise the exit code is 1. |
| `0xFF` | `NOP` | No operation. |

## Stack Bytecodes

These bytecodes are used for mundane `S`-stack manipulations.
Note that while `LLCL` can access all values on the stack
it needs an argument which takes space in the bytecode sequence and must
also do some computation to find the right value and retrieve it.
Alternative bytecodes such as `DUP`, `DUPx1`, `DUPx2`, `SWAP`, and `OVER`
are significantly faster than the equivalent sequences with `LLCL`
(not to mention easier for a human to debug).

| Byte | Name | Arguments ([byte count]:[name]) | Operation |
|------|------|-----------|-----------|
| `0x01` | `NIL` | none | `<S> -> <nil:S>` |
| `0x02` | `I0` | none | `<S> -> <0:S>` |
| `0x03` | `I1` | none | `<S> -> <1:S>` |
| `0x04` | `BT` | none | `<S> -> <true:S>` |
| `0x05` | `BF` | none | `<S> -> <false:S>` |
| `0x06` | `LDH` | `1:H` | `<S> -> <H:S>`: push an `INT` object with value `H`. |
| `0x07` | `LDW` | `2:W` | `<S> -> <W:S>`: like `LDH` |
| `0x08` | `LDD` | `4:D` | `<S> -> <D:S>`: like `LDH` |
| `0x09` | `CHR` | `1:C` | `<S> -> <chr(C):S>` push a `CHAR` object representing the character with value `C`. |
| `0x0A` | `LDGW` | `2:BO` | `<S> -> <OBJ:S>`: push a pointer (not the byte offset) to the object at byte offset `BO`. |
| `0x0B` | `LDGD` | `3:BO` | `<S> -> <OBJ:S>`: push a pointer (not the byte offset) to the object at byte offset `BO`. |
| `0x0C` | `LLCL` | `1:H` | `<S> -> <locals[H]:S>` |
| `0x0D` | `SLCL` | `1:H` | `<x:S> -> <S>, locals[H]=x` |
| `0x0E` | `LDE` | none | `<S, E> -> <E:S, E>` |
| `0x0F` | `POP` | none | `<_:S> -> <S>` |
| `0x10` | `DUP` | none | `<x:S> -> <x:x:S>` |
| `0x11` | `TUCK` | none | `<x:y:S> -> <x:y:x:S>` |
| `0x12` | `TUCK2` | none | `<x:y:z:S> -> <x:y:z:x:S>` |
| `0x13` | `SWAP` | none | `<x:y:S> -> <y:x:S>` |
| `0x14` | `OVER` | none | `<x:y:S> -> <y:x:y:S>` |

Some specialized versions of `LLCL`, and `SLCL` are defined
in the "Shortened Bytecodes" section.

## Arithmetic & Logic Bytecodes

Most of these bytecodes expect `INT` objects on top of the stack and
perform some manipulation on them. The tags are not checked. 
In the current version, this means that these operations can be used
with boolean or character arguments with the expected results,
but using them with other objects is dangerous.
Dynamically typed language compilers should provide wrapper functions
which perform the checks.

| Byte | Name | Arguments ([byte count]:[name]) | Operation |
| -----|------|---------------------------------|-----------|
| `0x18` | `ADD` | none | `<I2:I1:S> -> <I1+I2:S>` |
| `0x19` | `SUB` | none | `<I2:I1:S> -> <I1-I2:S>` |
| `0x1A` | `MUL` | none | `<I2:I1:S> -> <I1*I2:S>` |
| `0x1B` | `DIV` | none | `<I2:I1:S> -> <I1/I2:S>` |
| `0x1C` | `REM` | none | `<I2:I1:S> -> <I1%I2:S>` |
| `0x1D` | `NEG` | none | `<I:S> -> <-I:S>` |
| `0x1E` | `SHL` | none | `<I2:I1:S> -> < I1<<I2 : S >` |
| `0x1F` | `SHR` | none | `<I2:I1:S> -> < I1>>I2 : S >` |
| `0x20` | `ASR` | none | `<I2:I1:S> -> < I1>>>I2 : S >` |
| `0x21` | `INC` | `1:H, 1:V` | `locals[H] = (INT(I) -> INT(I+V))`, `V` is signed |
| `0x22` | `AND` | none | `<I2:I1:S> -> <I1&I2:S>` |
| `0x23` | `OR` | none | `<I2:I1:S> -> <I1\|I2:S>` |
| `0x24` | `XOR` | none | `<I2:I1:S> -> <I1^I2:S>` |

## Object Manipulation

It is sometimes useful to manipulate the tag of an object without
manipulating its contents, for example when crafting literal characters
or when an object can be created by cloning an existing one and
modifying it (which is sometimes faster than making a new one from
scratch).

| Byte | Name | Arguments ([byte count]:[name]) | Operation |
| -----|------|---------------------------------|-----------|
| `0x28` | `TAG` | none | `<OBJ:S> -> <TAG(OBJ):OBJ:S>` |
| `0x29` | `STAG` | `2:W` | `<OBJ:S> -> <OBJ:S>, TAG(OBJ)=W` |
| `0x2A` | `I2B` | none | convert the INT object on top of the stack to a BOOL object |
| `0x2B` | `I2C` | none | convert the INT object on top of the stack to a CHAR object |
| `0x2C` | `B2I` | none | convert the BOOL object on top of the stack to an INT object |
| `0x2D` | `C2I` | none | convert the INT object on top of the stack to a BOOL object |
| `0x2E` | `ALLOC` | `2:size` | Allocate a heap object of the given size and push it to the stack. Its tag must be set by `STAG` and its fields by `SFLD` or `SFLDW`. Used to support large objects. |
| `0x2F` | `MKOBJ` | `1:n, 2:tag` | `<F1:...:Fn:S> -> <OBJ(tag,F1,...,Fn):S>` |
| `0x30` | `MKCLO` | `1:n, 3:CP, 1:A` | `<V1:...:Vn:S> -> <FUN(CP,A,V1,...,Vn):S>` |
| `0x31` | `MKLCLO` | `1:n, 3:CP, 1:A` | `<V1:...:Vn-1:S> -> <FUN(CP,A,E,V1,...Vn-1):S>` |
| `0x32` | `CLONE` | none | `<OBJ:S> -> <SHALLOWCOPY(OBJ):S>` |
| `0x33` | `UNPCK` | none | `<OBJ(tag,F1,...,Fn):S> -> <F1:...:Fn:S>` |
| `0x34` | `LFLD` | `1:n` | `<OBJ(_,...,Fn,...):S> -> <Fn:S>` "Load Field" |
| `0x35` | `LFLDW` | `2:n` | `<OBJ(_,...,Fn,...):S> -> <Fn:S>` |
| `0x36` | `SFLD` | `1:n` | `<V:OBJ(_,...,Fn,...):S> -> <OBJ(_,...,V,...):S>` |
| `0x37` | `SFLDW` | `2:n` | `<V:OBJ(_,...,Fn,...):S> -> <OBJ(_,...,V,...):S>` |
| `0x38` | `LDCV` | `1:n` | `<S, E=FUN(_,_,...,Vn,...)> -> <Vn:S, E>` "Load Direct Captured Value" |
| `0x39` | `LLCV` | `1:L, 1:n` | `<S, E> -> <LL(E,L,n):S, E>` "Load Linked Captured Value", see comments below. |

The bytecodes `I2B`, `I2C`, `B2I`, and `C2I` do not check the input type,
they simply assume it. With the current internal representation, this means
that `B2I` and `C2I` are equivalent, and all 4 operations are just shortened
versions of `STAG`.

The `SFLD` and `SFLDW` bytecodes mutate an object. They should be used with care!
If the pointer to the mutated object might be shared, and only this object should
change (e.g. because values in the source language are immutable), then these
bytecodes are not appropriate. Instead, `CLONE` the object, and mutate the clone,
or build a new object with `MKOBJ`, depending on how many fields need to be
modified.

The `MKOBJ` and `MKCLO` bytecodes construct objects. `MKOBJ` can construct any
object, except a closure. (The first field of a closure is a literal integer,
not a pointer, and there is no way to express this as an operand of `MKOBJ`.)
`MKCLO` is provided to make closures specifically. In both cases, the fields
of the object must be pushed **in reverse order**, because hardware stacks
grow towards lower addresses and so this means that the fields on the stack
are actually in the right order in memory.

The `ALLOC` bytecode can be used to allocate large objects, but this should
never be necessary. Something is deeply wrong if a closure captures so many
values, and we have no way to support array indexing (currently!), so there
should be no reason for an object to have more than about 6 fields.

The `LLCV` bytecode operates on what is known as a "linked" closure.
The first field after the CP/Arity word in such a closure is a (a pointer to)
another closure, rather than the first captured variable. This pointer is also
called the link pointer. Such a closure looks like
`FUN(CP,A,P,...)`. The operation `LL(E,L,n)` is defined recursively as follows:
```
LL(FUN(_,_,F1,...,Fn,...), 0, n) = Fn
LL(FUN(_,_,P,...), L, n) = LL(P, L-1, n)
```
In other words, as long as `L` is not zero, decrease `L` and continue operating
on the link pointer of the given closure. Once `L` is zero, act as `LDCV n`
would if `E` were the given closure. Take note of this asymmetry: on a linked
closure, `LLCV 0 0` will push the link pointer itself, not the first captured
variable. (`LDCV` would do the same, since it doesn't expect linked closures.)

A compiler is free to mix linked and unlinked closures but is responsible for
keeping track of their layouts. A closure that is linked to by another need
not be linked itself (this is why `LLCV` does not assume a link pointer at the
last level).

Consider a function like the following:
```ml
let mk_inc_factory (n : int) =
  let mk_inc () =
    fun (m : int) -> m + n
```
This is contrived but it can be seen that the inner lambda captures a variable
from its context within `mk_inc`, which is in turn captured from the context in
`mk_inc_factory` in which this `mk_inc` closure was created. In this case,
the compiler is free to chose. The `mk_inc` closure will certainly not be
linked, and `LDCV 0` with it in `E` will produce the value of `n`.
To construct the closure for the anonymous function, we can either produce
a "direct" closure with `LDCV 0; MKCLO CP 1 1`, or a "linked" closure
with `MKLCLO CP 1 1`. Call the resulting closure `E'`.
When `E'` is entered, if it is direct, `LDCV 0` will produce the value of `n`.
If it is linked, then `LLCV 1 0` will produce the value of `n`.

The main performance consideration should be the size of the closures.
In this case, they are the same, and we should prefer simply capturing
`n` again. The bytecode sequence to do this is two bytes longer, but the
sequence to access the value whenever the resulting closure is called
is one byte shorter, and uses the faster `LDCV`. The enhanced speed of
`LDCV` also justifies direct closures if the size difference would be
small. However, once captures in closures start to become very nested,
every variable captured "direct" creates a linear growth in the size
of the created closures, and therefore a _quadratic_ growth in both
runtime to construct the closures and memory residency. Linked closures,
in contrast, would remain a constant size, and the execution time and
memory residency remain linear in the complexity of the program itself.
This is especially important for compilers that do a CPS transformation,
as continuations are a textbook source of nested variable capture.

Several specialized variations of `LFLD` and `LDCV` are defined in the
"Shortened Bytecodes" section.

## Intra-Function Control Flow

A suite of branching bytecodes is provided, in both compare-to-zero
and compare-to-value form. All comparisons are signed.
They do not perform type checks and so are
suitable for integers, booleans, and characters. Another comparison
tests if two objects are in fact the same object. Two final pairs of
bytecodes test specifically for the boolean `false`, or for a pythonic
notion of "falsy" which includes `nil` and all immediate objects
whose fields are the word `0`. (The integer 0, the boolean `false`,
and the character `\0`.) It is not safe to use `IFEQ` or similar bytecodes
when the top object of the stack might be `nil`. It is safe to use
`IFF`, `IFNF`, `IFSAME`, `IFDIFF`, `IFFALSY`, and `IFTRUTHY`.

Branch targets are signed 16-bit relative
offsets from the bytecode offset of the branching bytecode.
Branches closer to the code size limit can be implemented with a short
branch and a long `GOTO`.
When branches are taken, their effect on the code pointer is to
add the relative offset `RO`.

| Byte | Name | Arguments ([byte count]:[name]) | Operation |
| -----|------|---------------------------------|-----------|
| `0x40` | `IFEQ` | `2:RO` | `<V:S> -> <S>`, branch if the first field of `V` is `0`. |
| `0x41` | `IFNE` | `2:RO` | `<V:S> -> <S>`, branch if the first field of `V` is not `0`. |
| `0x42` | `IFLT` | `2:RO` | `<V:S> -> <S>`, branch if the first field of `V` is less than `0`. |
| `0x43` | `IFGT` | `2:RO` | `<V:S> -> <S>`, branch if the first field of `V` is more than `0`. |
| `0x44` | `IFLE` | `2:RO` | `<V:S> -> <S>`, branch if the first field of `V` at most `0`. |
| `0x45` | `IFGE` | `2:RO` | `<V:S> -> <S>`, branch if the first field of `V` at least `0`. |
| `0x46` | `IFF` | `2:RO` | `<V:S> -> <S>`, branch if `V` is the boolean `false`. |
| `0x47` | `IFNF` | `2:RO` | `<V:S> -> <S>`, branch if `V` is not the boolean `false`. |
| `0x48` | `IFCEQ` | `2:RO` | `<V2:V1:S> -> <S>`, branch if `V1` and `V2` are equal immediate objects (they must not be `nil`). Does not work on strings, closures, etc. |
| `0x49` | `IFCNE` | `2:RO` | `<V2:V1:S> -> <S>`, branch if `V1` and `V2` are not equal as immediate objects. |
| `0x4A` | `IFCLT` | `2:RO` | `<V2:V1:S> -> <S>`, branch if `V1 <  V2`. |
| `0x4B` | `IFCGT` | `2:RO` | `<V2:V1:S> -> <S>`, branch if `V1 >  V2`. |
| `0x4C` | `IFCLE` | `2:RO` | `<V2:V1:S> -> <S>`, branch if `V1 <= V2`. |
| `0x4D` | `IFCGE` | `2:RO` | `<V2:V1:S> -> <S>`, branch if `V1 >= V2`. |
| `0x4E` | `IFSAME` | `2:RO` | `<V2:V1:S> -> <S>`, branch if `V1` and `V2` are the same object. |
| `0x4F` | `IFDIFF` | `2:RO` | `<V2:V1:S> -> <S>`, branch if `V1` and `V2` are not the same object. |
| `0x50` | `IFNIL` | `2:RO` | `<V:S> -> <S>`, branch if `V` is `nil`. |
| `0x51` | `IFNNIL` | `2:RO` | `<V:S> -> <S>`, branch if `V` is not `nil`. |
| `0x52` | `IFFALSY` | `2:RO` | `<V:S> -> <S>`, branch if `V` is `nil` or if the first field of `V` is the word `0`. |
| `0x53` | `IFTRUTHY` | `2:RO` | `<V:S> -> <S>`, branch if `IF_FALSY` would not. |

A single `GOTO` operation which can jump anywhere in the program is provided.
`GOTO` outside of the current function is presumed not to be useful,
as even a compiler which produces CPS code will need to use `ETL`.

| Byte | Name | Arguments ([byte count]:[name]) | Operation |
| -----|------|---------------------------------|-----------|
| `0x54` | `GOTO` | `3:CP` | `<C=_> -> <C=CP>` |


Several forms of tag-dependent code
flow are provided for use by compilers as desired. They are
`MATCH`, `MATCHD`, `CHKTAG`, and some more tag-aware branches.
I currently anticipate that statically typed
source languages will primarily use `MATCH` and dynamic languages primarily
`CHKTAG`, to implement dynamic type checks which halt with an error on failure.
(The error messages are of the form "Type error: expected X, got Y.")
The tag-aware branches are an alternative to `MATCH` for
types with few constructors.

Additionally, the `TAG` bytecode pushes the tag of the
object on top of the stack for more fine-grained control if needed.
(It is pushed as an INT object.)

| Byte | Name | Arguments ([byte count]:[name]) | Operation |
| -----|------|---------------------------------|-----------|
| `0x55` | `MATCH` | `0-3: padding, 2:N, [4:BO]*` | `<V:S> -> <V:S>`. See pattern matching below. |
| `0x56` | `MATCHN` | `0-3: padding, 2:N, 4:NIL, [4:BO]*` | `<V:S> -> <V:S>`. See pattern matching below. |
| `0x57` | `MATCHD` | `0-3: padding, 2:LO, 2:HI, 4:DEF, [4:BO]*` | `<V:S> -> <V:S>`. See pattern matching below. |
| `0x58` | `MATCHND` | `0-3: padding, 2:LO, 2:HI, 4:NIL, 4:DEF, [4:BO]*` | `<V:S> -> <V:S>`. See pattern matching below. |
| `0x59` | `CHKTAG` | `2:tag` | `<OBJ(tag,...):S> -> <OBJ:S>`, otherwise halt with an error. |
| `0x5A` | `IFTEQ` | `1:tag, 2:RO` | `<OBJ(tag',...):S> -> no change`, Branch if `tag' == tag`. |
| `0x5B` | `IFTLT` | `1:tag, 2:RO` | `<OBJ(tag',...):S> -> no change`, Branch if `tag' < tag`. |

The `MATCH{N,D}` bytecodes implement jump tables.
For `MATCH` and `MATCHD`, the top value cannot be nil.
For `MATCHN` and `MATCHND`, the `NIL` target is taken iff the top value
on the stack is nil.
In all other cases, the value to switch on is the value on top of the stack. 
If that object is an immediate object, its field is used for the switch.
(This feature might be removed if it turns out to be expensive and useless.)
Otherwise, the object's tag is used. Call the value to switch on `T`.

`MATCHD` can implement pattern matching in a dynamically typed language,
or whenever else a default branch or generic range of tags is useful.
Immediately following the bytecode are the necessary number of 0 bytes to align
the remaining arguments to a 4-byte offset boundary. `DEF` is then a 4-byte
code pointer (so the most significant byte should be zero). The value
`LO` is the tag (or immediate) value that should correspond to jump table
index 0. In the immediate case, `LO` (and `HI`) are treated as signed.
`HI-LO+1` is the number of addresses in the jump table. Finally, each address
`BO` is a 4-byte code pointer like `DEF`.

If `T` falls outside the range `[LO,HI]`, control is
transferred to `DEF`. Otherwise, control is transferred to the code pointer
at index `T-LO` of table.

`MATCH` operates as `MATCHD`, except that `LO` is always `0` if `T` is a
field of an immediate object, and `16` if `T` is an object's tag.
The value `N` is the number of labels in the table, and does not count
`MATCHN`'s label for nil.
No default label is given as the match must be statically proven to be
complete. (For an ML-style language, the compiler can insert code to
invoke an error routine for cases un-matched in the source program.)
The padding aligns to a 2-mod-4 byte offset rather than 0-mod-4,
so that the addresses in the table will be 4-byte aligned.

Consider the ML function
```ml
type ('a,'b) foo = Left of 'a | Right of (int * 'b);;
let forget = function
  | Left     x  -> x
  | Right (_,x) -> x;;
```
The extra `int` in the `Right` constructor is just so that the fields of
interest have different offsets -- otherwise we would not need a matching
bytecode at all.

If this function began at offset 0 (which is not possible),
and assuming that `Left` is given the tag 16 and `Right` the tag 17,
it could be implemented as
```
0: MATCH 0 [0 2] [0 0 0 12] [0 0 0 15] # padding, N=2, tgts 12 and 15
12: LFLD 0
14: RET
15: LFLD 1
17: RET
```
It should be noted that, when space is a concern, `MATCH` bytecodes can waste
significant space compared to `IFTEQ` or `IFTLT` branches. While the `MATCH`
might be faster, this comes with tradeoffs in space. We could instead compile
the above function as the very straightforward:
```
0: IFTEQ 17 7
4: LFLD 0
6: RET
7: LFLD 1
9: RET
```
For more complex datatypes, `IFTLT` can be used to build a binary search tree
for the correct case alternative. Each `IFTLT` bytecode requires 4 bytes
in the instruction stream (notice that they use single-byte tags) which is
the same space required by a `MATCH` alternative, so the savings do not scale
at all with the number of case alternatives, unfortunately.

Note: I am still thinking through this and waiting to measure it, but it may be
the case ignoring padding and using 3-byte offsets in `MATCH` instructions
would be just about right so that `MATCH` takes less space and less time
than moderately large search trees, but small search trees are still better.
(There is a time cost here, because the target machine does not allow
misaligned memory access.)

### Another Example

Consider the ML list data type:
```ml
type 'a list = [] | (::) of ('a * 'a list);;
```

This type has exactly one nullary constructor. For such types, the nullary
constructor can be represented by `nil`, which saves both time and space.
Each case alternative for `MATCH` takes four bytes, but `IF_NIL` takes only
three. Additionally, when there are only two constructors, `IF_NIL` is the
only test needed to resolve a pattern match, and is much faster than `MATCH`.

For this function:
```ml
let rec sum acc xs = match xs with
  | [] -> acc
  | (x::xs') -> sum (x+acc) xs'
```
we can therefore generate this rather elegant bytecode:
```
0: 0x00 0x05 0x00 0x0B 0x00000802 # FUN(8,2)
8: LLCL0        # list being matched
9: DUP          # copy
10: IF_NNIL +5  # if it is nil...
13: LLCL1        # push acc
14: RET          # and return it
15: UNPCK       # otherwise, unpack :: ( x:xs':acc:xs:S )
16: LLCL1       # push acc
17: ADD         # add acc+x ( (acc+x):xs':acc:xs:S )
18: FASTTL 0    # ( (acc+x):xs':S ) and invoke recursively
```
If code space is more of a concern than speed, the final `FASTTL` can also be
implemented as `LDE; UTL`.

## Inter-Function Control Flow

A large variety of function calling operations is provided. Operational
semantics are clarified below where necessary.

| Byte | Name | Arguments ([byte count]:[name]) | Operation |
| -----|------|---------------------------------|-----------|
| `0x60` | `FASTAP` | `3:BO` | `<S,E,C,D> -> <S, *BO=FUN(CP,A,...), CP, (drop A S,E,C+3):D>` |
| `0x61` | `FASTTL` | `3:BO` | `<S,_,_,(S',E',C'):D> -> <take A S ++ S', *BO=FUN(CP,A,...), CP, (S',E',C'):D>` |

These two bytecodes provide fast calls to known functions. Provided is the
offset of the static closure for that function.

The `FASTTL` bytecode provides a fast tail-call. The semantics are expressing
that any activity that occurred on the stack between the saved stack pointer
`S'` and the parameters on the stack (the number of which is determined by
checking the arity of the closure) is erased. The parameters are slid down
to be just above the saved pointer (in other words, to the location where
the current function's arguments are) and control is transferred to the
closure indicated by `BO`. This has the effect of replacing the current
evaluation context with that indicated by `BO`, in particular without creating
a new frame on the dump.

| Byte | Name | Arguments ([byte count]:[name]) | Operation |
| -----|------|---------------------------------|-----------|
| `0x62` | `EAP` | `1:P` | `<F=FUN(CP,A,...):S,E,C,D> -> <S, F, CP, (drop A S,E,C+2):D>` |
| `0x63` | `ETL` | `1:P` | `<F=FUN(CP,A,...):S,_,_,(S',E',C'):D> -> <take A S ++ S', F, CP, (S',E',C'):D>` |
| `0x64` | `EAP0` | none | As `EAP 0` |
| `0x65` | `EAP1` | none | As `EAP 1` |
| `0x66` | `EAP2` | none | As `EAP 2` |
| `0x67` | `EAP3` | none | As `EAP 3` |
| `0x68` | `EAP4` | none | As `EAP 4` |
| `0x69` | `EAP5` | none | As `EAP 5` |
| `0x6A` | `ETL0` | none | As `ETL 0` |
| `0x6B` | `ETL1` | none | As `ETL 1` |
| `0x6C` | `ETL2` | none | As `ETL 2` |
| `0x6D` | `ETL3` | none | As `ETL 3` |
| `0x6E` | `ETL4` | none | As `ETL 4` |
| `0x6F` | `ETL5` | none | As `ETL 5` |

`EAP` and `ETL` provide uncurried (tail) calls. The arity of the closure on top
of the stack is checked against `P`. If they match, then the operation shown
is performed. The operation is analagous to `FASTAP` and `FASTTL` except that
the closure to apply is taken from the stack.
If the arity does not match `P`, the program is halted with an error.
A dynamically typed language must ensure that the object is a closure before
using these bytecodes.

These bytecodes have compressed forms for zero to five arguments,
which cover the vast, vast majority of function calls.

| Byte | Name | Arguments ([byte count]:[name]) | Operation |
| -----|------|---------------------------------|-----------|
| `0x70` | `UAP` | none | As `EAP`, but without checking against `A` |
| `0x71` | `UTL` | none | As `ETL`, but without checking against `A` |
| `0x72` | `RET` | none | `<V:_,_,_,(S',E',C'):D> -> <V:S',E',C',D>` |
| `0x73` | `SYS` | `1:H` | Invoke system function with code `H`. |
| `0x74` | `CAP1` | none | See below, `P = 1` |
| `0x75` | `CAP2` | none | See below, `P = 2` |
| `0x76` | `CAP3` | none | See below, `P = 3` |
| `0x77` | `CAP4` | none | See below, `P = 4` |
| `0x78` | `CAP5` | none | See below, `P = 5` |
| `0x79` | `CAP6` | none | See below, `P = 6` |
| `0x7A` | `CTL1` | none | See below, `P = 1` |
| `0x7B` | `CTL2` | none | See below, `P = 2` |
| `0x7C` | `CTL3` | none | See below, `P = 3` |
| `0x7D` | `CTL4` | none | See below, `P = 4` |
| `0x7E` | `CTL5` | none | See below, `P = 5` |
| `0x7F` | `CTL6` | none | See below, `P = 6` |

`UAP` and `UTL` provide uncurried closure application without arity checks;
which can be useful for a language which checks arity statically.

`RET` returns from a function, restoring the saved context and pushing
the return value on top of that context's stack.

`SYS` calls an interpreter service function with the given code, to provide
things like IO or exiting. That service function uses the `S` stack if
necessary.

The `CAP` and `CTL` bytecode families apply function-like objects to
arguments with a curried semantics.

The arity of the closure (resp. PAP) is checked against `P`. If the arity
is larger, a PAP is constructed which fixes the pointer to the original
closure and the supplied arguments; this PAP is the result of the
application. (The exact format of PAPs is not currently specified.
You should not assume that there is a connection between PAP A, constructed
from partially applying PAP B, and PAP B. Neither should you assume that
there is _not_ such a connection.)

If the arity is exactly `P`, then the function-like object is entered.
A closure is entered as if by `UAP` or `UTL` as appropriate
To enter a PAP, the fixed arguments in the PAP are pushed onto the stack
in reverse order, and we proceed by dealing with the fixed closure instead
of the PAP. This process is known as "zonking" a PAP.^1
This closure is then entered.

If the arity is less than `P`, the function-like object is entered
and is assumed to return another function-like object. When it does,
it is applied to the remaining arguments in a recursive fashion.

The arity-is-greater and arity-is-equal cases are straightforward enough,
but if one thinks about it for a bit, it becomes quite clear that it is
very difficult to implement the arity-is-less case in a bytecode interpreter!
The problem is that the function-like object must be entered
and the bytecode must _continue to control the state transition after
it returns_.

Currently, the arity-is-less case is implemented by first determining if the
function-like object is a PAP; if so, it is zonked before proceeding.
Then, an operation like `FASTAP` or `FASTTL` (for `CAP` and `CTL` respectively)
is performed to transfer control to an internal bytecode sequence that will
regain control after the first application is complete.
It is easier to understand if these cases are considered separately.

In the `CAP` case, a dump frame is created for the current context, except
the saved stack pointer is adjusted (by `P`) to be below the arguments that
will be consumed by the operation. Then
and control is transferred to an internal bytecode sequence. The stack and
current environment pointer are unchanged. The internal bytecode sequence
is `UAP; CTL<P-A>`. If we tried to apply a closure of arity 1 to 2 parameters,
for example, the sequence will be `UAP; CTL1`. The use of `UAP` will cause
the closure of arity 1 to be entered (and it will always be a closure,
because we've zonked it), creating a new (second!) dump frame for the
context whose code pointer is our internal bytecode sequence.
When that closure returns, it will therefore return to our internal bytecode
sequence, with the number of arguments that it consumed (1 in the example)
removed from the stack and another function-like object pushed. The dump
frame created at the start still exists. We then use `CTL1` to apply the
function-like object just returned to the remaining argument.
We use a curried call because that function-like object may have an
arity other than 1, and we use a tail call because the context that we want
it to return to is the context of the original `CAP` bytecode, with all
of the `P` consumed arguments removed from the stack, which is precisely
the context that we saved on the dump at the start.

In the `CTL` case, however, we don't want that last `CTL1` to return to
the context of the original `CTL` bytecode, because that bytecode is a
tail calling bytecode. We want it to return to the context on the stack
before we began. Well, that's easy! We simply don't create a new dump
frame before transferring control to the internal bytecode sequence.
Everything else is identical.

The requirement of having a static internal bytecode sequence to invoke
is precisely why we do not provide generic `CAP` or `CTL` bytecodes.
If a program requires a `CAP8`, it can instead `CAP6`, then `CAP2`.
(A compiler may, of course, guess a better decomposition.)

## Shortened Bytecodes

| Byte | Name | Arguments ([byte count]:[name]) | Operation |
| -----|------|---------------------------------|-----------|
| `0x80` | `LLCL0` | none | as `LLCL 0` |
| `0x81` | `LLCL1` | none | as `LLCL 1` |
| `0x82` | `LLCL2` | none | as `LLCL 2` |
| `0x83` | `LLCL3` | none | as `LLCL 3` |
| `0x84` | `LLCL4` | none | as `LLCL 4` |
| `0x85` | `LLCL5` | none | as `LLCL 5` |
| `0x86` | `LLCL6` | none | as `LLCL 6` |
| `0x87` | `LLCL7` | none | as `LLCL 7` |
| `0x88` | `LLCL8` | none | as `LLCL 8` |
| `0x89` | `LLCL9` | none | as `LLCL 9` |
| `0x8A` | `SLCL0` | none | as `SLCL 0` |
| `0x8B` | `SLCL1` | none | as `SLCL 1` |
| `0x8C` | `SLCL2` | none | as `SLCL 2` |
| `0x8D` | `SLCL3` | none | as `SLCL 3` |
| `0x8E` | `SLCL4` | none | as `SLCL 4` |
| `0x8F` | `SLCL5` | none | as `SLCL 5` |
| `0x90` | `SLCL6` | none | as `SLCL 6` |
| `0x91` | `SLCL7` | none | as `SLCL 7` |
| `0x92` | `SLCL8` | none | as `SLCL 8` |
| `0x93` | `SLCL9` | none | as `SLCL 9` |
| `0x94` | `LFLD0` | none | as `LFLD 0` |
| `0x95` | `LFLD1` | none | as `LFLD 1` |
| `0x96` | `LFLD2` | none | as `LFLD 2` |
| `0x97` | `LFLD3` | none | as `LFLD 3` |
| `0x98` | `SFLD0` | none | as `SFLD 0` |
| `0x99` | `SFLD1` | none | as `SFLD 1` |
| `0x9A` | `SFLD2` | none | as `SFLD 2` |
| `0x9B` | `SFLD3` | none | as `SFLD 3` |
| `0x9C` | `LDCV0` | none | as `LDCV 0` |
| `0x9D` | `LDCV1` | none | as `LDCV 1` |
| `0x9E` | `LDCV2` | none | as `LDCV 2` |
| `0x9F` | `LDCV3` | none | as `LDCV 3` |
| `0xA0` | `LLCV10` | none | as `LLCV 1 0` |
| `0xA1` | `LLCV11` | none | as `LLCV 1 1` |
| `0xA2` | `LLCV12` | none | as `LLCV 1 2` |
| `0xA3` | `LLCV13` | none | as `LLCV 1 3` |
| `0xA4` | `LLCV20` | none | as `LLCV 2 0` |
| `0xA5` | `LLCV21` | none | as `LLCV 2 1` |
| `0xA6` | `LLCV22` | none | as `LLCV 2 2` |
| `0xA7` | `LLCV23` | none | as `LLCV 2 3` |

-----------------------------------------

[^1]: This term comes by allusion from "zonking" in the implementation
of a type inference engine. There, a half-baked type which still contains
"holes" of unknown types contains a mutable reference in place of the holes.
When the correct hole-filling type is discovered, it is written into the
reference. "Zonking" walks over a type that might by half-baked and,
wherever there is a reference that has been filled, replaces that ref by the
type it was filled with. We are doing something similar here: the PAP
represents an application with holes for several arguments.
We are unpacking that half-baked application back onto the stack
and then proceeding as if the original closure had been applied to all
of the arguments (both the previously-fixed ones and the new ones)
all at once. I believe I have also seen this referred to as "unwinding"
a PAP, but I find this term confusing as it takes place on the stack
and causes the stack to _grow_, whereas unwinding typically shrinks the stack.
By extension, we can "zonk" any function-like object, having no effect on
closures but zonking a PAP. The result of "zonking the application head"
therefore will be a possibly-modified machine state where the application
head is certainly a closure.
