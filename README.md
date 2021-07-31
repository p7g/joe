# joe

I watched @jonhoo's video about [Dispatch and Fat Pointers][1] recently, and it
answered a lot of questions I had about how vtables work with subclassing and
interfaces.

As such, I felt compelled to try it out, and this is my attempt to do so.
Currently it only compiles the most basic of statements, but it already
generates a data struct and a vtable struct (and variable) per class.

[1]: https://www.youtube.com/watch?v=xcygqF5LVmM

## What's the plan?

What's a plan?

Eventually I'll implement interfaces and subclassing. Every interface
implementation will result in a separate vtable. When an object is passed as an
instance of the interface, it will be converted to a fat pointer holding the
object and the vtable for the interface implementation. The same methods in the
interface implementation will probably also exist in the class's vtable for
ease of implementing calls to those methods from within the class.

Subclasses will define their own vtables, but they will do so such that the
superclass's vtable is a prefix of their vtable (i.e. it's safe to cast from
the subclass's vtable to the superclass's vtable).

## What does it do now?

Given a file `simple.java` like this:

```java
final class simple {
    final int _n;

    void simple(int n) {
        _n = n;
    }

    int test(int a) {
        return a + _n;
    }

    static void main() {
        simple s;
        simple s2 = new simple(123);

        println(s2.test(234));
        delete s2;
    }

    static void println(int n) {}
}
```

Running `python -m joe --main simple.main simple.java` results in the following
C code:

```c
#include <stdlib.h>
typedef int __joe_N6simple_data;
typedef __joe_N6simple_data __joe_N6simple;
static void __joe_N6simple6simpleIivEE(__joe_N6simple* self, int __joe_L1n);
static int __joe_N6simple4testIiiEE(__joe_N6simple self, int __joe_L1a);
static void __joe_N6simple4mainIvEE();
static void __joe_N6simple7printlnIivEE(int __joe_L1n);
int main();
static void __joe_N6simple6simpleIivEE(__joe_N6simple* self, int __joe_L1n) {
    ((self)[0]) = (__joe_L1n);
}
static int __joe_N6simple4testIiiEE(__joe_N6simple self, int __joe_L1a) {
    return (__joe_L1a) + (self);
}
static void __joe_N6simple4mainIvEE() {
    __joe_N6simple __joe_L1s;
    __joe_N6simple __joe_L2s2;
    __joe_N6simple __joe_tmp_2;
    int __joe_tmp_3;
    __joe_N6simple6simpleIivEE(&(__joe_tmp_2), 123);
    (__joe_L2s2) = (__joe_tmp_2);
    (__joe_tmp_3) = (__joe_N6simple4testIiiEE(__joe_L2s2, 234));
    __joe_N6simple7printlnIivEE(__joe_tmp_3);
}
static void __joe_N6simple7printlnIivEE(int __joe_L1n) {
}
int main() {
    __joe_N6simple4mainIvEE();
    return 0;
}
```

It's not pretty, but it does compile, and it ~sure does print `357\n`~ doesn't
print anything because I need to figure out C interop.


### Tell me about the cool stuff shown in that C above

Since the `simple` class is final, the compiler doesn't even generate a vtable
for it. Every method call on that class is statically dispatched.

On top of that, since there is only one field in the class, the compiler can
avoid creating a data struct, instead just using a pointer to the single
field's type as the object data (this should have no performance benefit; a
single-field struct has the same runtime representation as the type of the
single field).

On top of _that_, since the single field is `final`, the compiler can use the
type of the field as the object data (in this case the constructor is passed a
reference to `self` so that the stack value can be initialized).

The end result is that the `simple` class in the above example is just a
typedef of `int`.

This justifies having `final` despite not yet having inheritance because it
results in the following assembly on my system (when the C is compiled with
`-O3`) (and when I've manually added a call to `printf` in the `println`
method):

```asm
	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 11, 0	sdk_version 11, 3
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	leaq	L_.str(%rip), %rdi
	movl	$357, %esi                      ## imm = 0x165
	xorl	%eax, %eax
	callq	_printf
	xorl	%eax, %eax
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"%d\n"

.subsections_via_symbols
```

Which, thanks to the C compilers that are much better than this humble one, is
equivalent to a program like this:

```c
#include <stdio.h>

int main(void) {
    printf("%d\n", 357);
    return 0;
}
```

## What's next?

The next optimization on my radar (yes I know you can't even write an `if`
statement yet...) is to, in cases where a class has only a single data member
and is final, reduce the "data" part of an object to just the single field's
type. In the example above, the object representation would become just `int`.
(DONE)

This, plus the addition of "unchecked" arrays (i.e. just a pointer under the
hood) and generics and destructors etc., would allow for a class like this:

```java
final class Pointer<T> {
    unchecked T[] _data;

    void Pointer<T>(unchecked T[] data) {
        _data = data;
    }

    // invoked by `delete ptr_obj;`
    ~Pointer() {
        delete _data;
    }

    T read() {
        return readAt(0);
    }

    T readAt(int pos) {
        return _data[pos];
    }

    void write(T value) {
        writeAt(0, value);
    }

    void writeAt(int pos, T value) {
        _data[pos] = value;
    }
}
```

If you're thinking what I'm thinking, this seems like a great way to implement
interop with C without introducing _actual_ pointers to the language (imagine
`Pointer<char>` for a second; the representation of it would literally just be
`char *`).
