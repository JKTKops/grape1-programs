# Bytecode Interpreter

This is a bytecode interpreter for an abstract machine suited to functional
languages without call/cc. The bytecode is taken as file input and loaded
into RAM somewhere in higher-half memory. The bytecode program is executed
in unprivileged mode to prevent system takeover by malicious (or innocently
invalid) bytecode programs.

# Intended Use Cases

This interpreter could be included as a tool in an operating system for
purposes of bootstrapping the environment, as it is notably faster to
run than the ETCa Forth system created for a similar purpose.
The tradeoff is that using this interpreter also requires the creation
of a bytecode compiler, whereas the Forth system is self-contained.
The ETCa bootstrapping series discussed as a long-term future plan may
write a bytecode compiler in Forth, use it to compile a bytecode compiler
in some other language, and then get rid of Forth that way.

It can also just be used to run bytecode programs on bare metal
for fun. Whatever floats your boat.

# Kernel Shim

A shim kernel is provided that gives the IO functionalities that the C code
uses from the C standard library (primarily `puts` and `exit`) in the form
of syscalls. The kernel has control at startup. It loads the bytecode
sequence into memory and then invokes the interpreter in user mode.
The syscall numbers are named in the assembly source and can be modified
for inclusion into another OS.

# Word size

We use a 32-bit word size, and 32-bit heap pointers. As is traditional
in ETCa contexts, 32-bit values are often abbreviated `d` and 16-bit
values are often abbreviated `w`, but we will use the term "word"
exclusively to mean "32-bit value."

# Limits

Pointers to data in the bytecode itself are 16-bit offsets within the bytecode.
This limits the location of such data to the first 64kb of the bytecode
program. However, code pointers in closure objects are 24-bit offsets,
so program bytecode up to 16 MB in length is supported.
Any larger than 24-bit offsets would have performance implications.

The target machine currently only supports 2MB of RAM in the region where
the bytecode is loaded, so there is therefore an artifical 2MB limit.
(Actually less, as all of the interpreter state is stored there as well.)

Do note that data in the bytecode cannot contain pointers to other data
in the bytecode. If such an object is required, the main function should
initialize it. Lifting this restriction is impossible
without complicating the bytecode loader to perform some primitive
load-time linking, which would also complicate the bytecode format.
A future version may do this.

There stack size limit. It can be configured when the interpreter is built.
(TODO: HOW?) Larger stack limits mean smaller heap sizes.

# Abstract Machine

This is an implementation of the SECD abstract machine,
which manipulates a state structure consisting of 4 fields:

1. Stack
2. Environment
3. Code (known in the literature as Control)
4. Dump

The SECD machine can be more accurately understood as a two-stack
machine where one stack (S) is used for evaluating expressions and
the other (D) is used as a sort of enriched return stack.
It is well-known that code for such machines can also be implemented
on a machine with one stack (corresponding _mostly_ to D) and some
registers; this would correspond to compiling your source language
to ETCa machine code directly. 

Readers familiar with Forth will notice some direct correlations.
Though we have a Forth implementation for ETCa, we are not using it
for this project. Future implementations of interpreted Lisps on ETCa,
bootstrapped by Forth, might wish to use a similar SECD model.

We wish to support both curried and uncurried functional languages.
To support the uncurried languages, each closure contains its arity.
For the curried languages, this has the nice effect of making it
straightforward to use an eval/apply evaluation model
([see](https://www.cs.tufts.edu/~nr/cs257/archive/simon-peyton-jones/eval-apply-jfp.pdf)),
which would otherwise require this arity anyways.^1

The bytecode stream contains both code and some data. The data are
entirely static objects for top-level values (functions included).
Their (16-bit) offsets may appear as arguments to some bytecodes.

## Uncurried Function Calls

In an uncurried functional language, it is always explicit exactly
how many arguments are being passed to a function, and it is similarly
explicit exactly how many arguments a function expects.

Consider this Scheme definition:
```scheme
(define (split-cons a b)
  (cons (car a) (cdr b)))
```
This function takes as arguments two linked lists, and forms a new list
whose head is the head of the first argument and whose tail is the tail
of the second argument. It takes two arguments. Calling it with a number
of arguments other than two is a runtime error. Similarly, the `cons`
builtin function in Scheme expects exactly two arguments, while the
`car` and `cdr` primitives expect exactly one.

For such "calls to known functions," the compiler is expected to generate
an error message if the given number of arguments is wrong at compile
time. The fast function call bytecode, namely `FASTAP` and `FASTTL`,
enter the closure on top of the stack which will simply operate
on the arguments left on the stack. The arity of the function is not
checked, because it must have been checked at compile time already.
(The arity of the function is _used_, however, to arrange the arguments.)

Consider, by contrast, this Scheme definition:
```scheme
(define (apply f x)
  (f x))
```
When generating code for `apply`, barring various inlining optimizations,
the code generator cannot know which function will be pointed to by the
closure `f` passed at runtime. It therefore cannot use the FASTTL
bytecode because the arity must be checked at runtime.
`apply` should instead be compiled using the `ETL` bytecode
("exact tail call").
This bytecode checks the arity of the closure on top of the stack
and compares it against an argument given in the bytecode stream indicating
how many arguments were given to the call. Mismatches cause runtime errors.
If the arity and actual counts match, the closure is entered.

Consider this definition:
```scheme
(define (curry2 f)
  (lambda (x)
  (lambda (y)
  (f x y))))
```
The result of `(curry2 apply)` is a closure expecting one argument `x`,
which when called, returns another closure expecting one argument `y`,
when when called, returns the value of `(apply x y)`. The call to `f`
in the code for the innermost closure must be compiled with `ETL 2`.
However, the expression `((curry2 apply) f x)` will cause a runtime error,
because the outer application will be compiled with `ETL 2`, and the actual
count of 2 will be checked against the arity of `(lambda (x) ...)`, which is 1.
In the uncurried languages, such a call must be `(((curry2 apply) f) x)`.

## Curried Function Calls

Curried functional languages, by contrast, assume that any function can be
applied to any number of arguments. (Equivalently, that all functions are
functions of one argument, which might return other functions. A literal
implementation of this interpretation would be infeasibly slow.)

If the arity of the function is too low,
then the function is called and it is assumed that it will return a function
that will then be applied to the rest of the arguments.
(If the arity of the returned function is also too low,
then the process repeats until there would be no arguments left,
at which point the arity can no longer be too low.)
If the arity is exactly right (including if the arity is 0 and 0 arguments
were given), then the function is called as normally expected.
If the arity is too high, however, then a special type of object known as a
PAP, or _partial application_, is constructed and no function call occurs.
The resulting PAP may be called later as if it were a closure.

All of this happens, including possible calling PAPs, when using the
curried application bytecodes `CAP1`, `CAP2`, `CTL1`, `CTL2`, etc.^2
but not when using the known application bytecodes (`FASTAP`
and `FASTTL`) or the uncurried application bytecodes (`EAP` and `ETL`).

Consider an ML function which applies a given function to two arguments:
```ml
let apply2 f x y = f x y
```
When calling `apply2` without enough arguments,
we must use a curried application bytecode,
because a call like `apply2 f x` is in fact perfectly valid, but does not
supply enough arguments. It would therefore fail the arity check implied
by `EAP` or `ETL`, and cause dangerous undefined behavior if
`FASTAP` or `FASTTL` tried to actually call the function.
Similarly, a call like `apply2 f x y z` can be valid (depending on the type
of `f`), and in these cases, we must use `CAP4` because too many arguments
are supplied and `CAP4`'s implementation will handle making the 3-argument
call, followed by a 1-argument call to the function returned.
(`CAP4` is smart enough to know that the returned function might be a PAP,
and this is handled just fine.)

In the case that the right number of arguments are supplied, however,
the call can be implemented by `FASTAP` or `FASTTL`, as we won't need
any such handling and we are effectively in the same situation as a
call in an uncurried language.

Hopefully, it is clear that the body of `apply2` **must** be compiled with
a `CAP2` instruction, because we cannot possibly know the arity of the
closure (or PAP!) that is received as the argument `f` at runtime.
Indeed, `apply2` may be called many times, with different values of `f`
_with different arities_.

## Calling Convention

All arguments are passed on the `S` stack, with the leftmost argument
at the top of the stack. Each function is responsible for cleaning up its
own stack space, including the space used by its arguments.
This is an easy task accomplished by the `RET` bytecode and every tail-call
bytecode.

# Object representation

The target machine accesses its memory nearly as fast as it does everything
else (it's a simulated machine, after all!) so there is no advantage to
tagging pointers. Actually, there's a disadvantage, as it would slow down
access to typical non-immediate objects.

The special pointer 0 represents nil, and can be re-used by a typed language
to represent any nullary constructor. Other objects are **always** pointers
and point to something on the heap with a representation like this:
```
(lower addr)                 (higher addr)
+----------------------------------------+
| tag,SIZE,S,1 | field 1 | field 2 | ... |
+----------------------------------------+
```
The first word of this object is metadata, containing a type tag,
the size of the object in words (shifted left 2 bits), a flag bit
known as the "static" bit, and then the least significant bit is set to 1.
The size in words shifted left 2 bits is in fact the size in bytes,
but the least significant bit is used by the garbage collector.
After that, the fields are each themselves objects
(either the null pointer or a pointer to another heap object)
and the number of fields depends on the thing that was allocated.
The type tag is not sufficient to determine the number of fields as
the tags are not limited to those given here.^3

Of the 32 bits in the first field, the more significant 2 bytes are
the type tag, the next 14 are the size (in words), and the remaining
bits are a flag and a one.

The flag indicates whether this object is "static," meaning it is
statically allocated. Such objects should never be garbage collected.
All objects on the heap will have a zero in this bit.
Most statically allocated objects are for top-level values
(including closures) included in the bytecode stream, see BYTECODE.md.
Additionally, to reduce pressure on the garbage collector, the machine
implementation provides several "canonical" objects for small integers
as well as the values `true` and `false` (`#t` and `#f` in Lisp parlance).
The garbage collector **will** forward objects to their canonical forms
if such a form is available, so it is not safe to mutate immediate
objects.

## Forms of Objects

Several tags are used for various immediate objects. While not necessary
for typed languages, untyped languages benefit from native support
for a few immediate object types; otherwise they would have to use
some other reserved tag for dynamic type checks and "spoof" an `INT`
object to take advantage of pattern matching; which would be unacceptable.
Immediate objects have one payload field, the immediate value.

* `INT` immediate objects have tag 0.
* `BOOL` immediate objects have tag 1, and a value of either 0 or 1.
* `CHAR` immediate objects have tag 2, and a value in [0,255].
* `STRING` objects have tag 3. The string content is in the object's
  payload, and is null-terminated.
* `FUN` objects have tag 4. The first field contains two packed values
  in its 32-bit word. The most-significant 24 bits are the code offset
  in the bytecode stream. The least-significant 8 bits are the arity.
  The remaining fields are the captured environment. If the compiler
  uses linked closure conversion, the link pointer should be the first
  field after the code offset and arity, which will enable use of the
  `LLCV` bytecode to load values from the closure (or linked closures).
  Further fields can be used to store the values of captured variables.
* `PAP` objects have tag 5. The first field is a pointer to a `FUN`
  object and the remaining fields are already-supplied arguments.
  The arity of a `PAP` object is determined dynamically using the stored
  size and the arity of the `FUN` object.
* Tags 6-15 are reserved for primitive object types in future versions.
* Tags above 16 may be used by the program for any allocated objects.

Generally, in typed languages, tags will correspond to the constructors
of an ADT. Compilers can freely reuse tags for different ADTs if the
compiler performs static typechecking, and may wish to avoid tag reuse
if dynamic type checks are required.

# Garbage Collection

Currently, the garbage collector is a simple nongenerational copying
collector. The single overhead word in every allocation contains all
of the information that it needs. Future versions will strive to maintain
this small overhead. As a result, future versions may lower the maximum
allocation size (the current limit of 16KB is hopefully quite excessive).

When an object is evacuated to the other semispace, the original copy's
header word is overwritten with a forwarding pointer to the new copy.
Since that pointer is necessarily aligned, its bottom bit is necessarily 0,
which is how the garbage collector identifies already-evacuated objects.


# Bytecode

Please see BYTECODE.md.

# Interpreter Service Functions

The `SYS` bytecode invokes service routines. Their codes are as follows:

TODO

-------------------------------------------------------------------

[^1]: Using eval/apply for stock hardware is experimentally most beneficial
because it makes it possible to pass function arguments in registers.
Unfortunately, being a bytecode interpreter, we do not get this benefit.
Eval/apply does still have some other advantages over push/enter, which we
are happy to exploit.

[^2]: In curried languages, it is not typically possible to call a function
with no arguments as there is no syntactic way to indicate this, and they
are typically typed by type systems which can not indicate it either.
Instead, such functions become single-argument functions who take the nil
value as their only argument and ignore it. This is why there are no
curried (tail-)call bytecodes with 0 arguments. Do note, however, that there
are bytecodes with dedicated implementations for specific small argument
counts in contrast to the `EAP` and `ETL` bytecodes for uncurried languages.
This is because functions with high argument counts are rare, and curried
functions can easily accomodate them by breaking what would be `CAP5` into
`CAP4` followed by `CAP1`.

[^3]: they are intended to
be used as the tags in tagged union code generated by a compiler
with static typic. For _dynamic_ typing, you will either need to generate
code that doesn't re-use tags or include dynamic type information in
an extra field of such objects, then do dynamic type checks. You may
wish to still use a few tags for the most common dynamic types even
in the latter case.
