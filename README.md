# joe

I watched @jonhoo's video about [Dispatch and Fat Pointers][1] recently, and it
answered a lot of questions I had about how vtables work with subclassing and
interfaces.

As such, I felt compelled to try it out, and this is my attempt to do so.
Currently it only compiles the most basic of statements, but it already
generates a data struct and a vtable struct (and variable) per class.

[1]: https://www.youtube.com/watch?v=xcygqF5LVmM

Unfortunately compilation of `new` expressions is not yet implemented, so
trying out method dispatching isn't yet possible lol

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

This is the output of running the `test.py` program against the `simple.java`
file right now:

```c
#include <stdio.h>
/* start module simple */
struct __joe6simple4dataE {
};
struct __joe6simpleE;
struct __joe6simple6vtableE {
};
struct __joe6simpleE {
    struct __joe6simple4dataE* data;
    struct __joe6simple6vtableE* vtable;
};
struct __joe6simple6vtableE __joe6simple6vtableE = {};

int __joe6simple4testE(int __joe6simple4test1aE) {
    return __joe6simple4test1aE;
}

void __joe6simple4mainE() {
    (printf)(("%d\n"), ((__joe6simple4testE)((23))));
}

int main() {
    (__joe6simple4mainE)();
    return 0;
}
/* end module simple */
```

It's not pretty, but it does compile, and it sure does print `23\n` :sunglasses:
