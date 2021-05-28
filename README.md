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

This is the output of running the `test.py` program against the `simple.java`
file right now:

```c
#include <stdio.h>
#include <stdlib.h>
/* start module simple */
struct __joe6simple4dataE {
    int _n;
};
struct __joe6simpleE;
struct __joe6simple6vtableE {
    void (*simple)(struct __joe6simpleE, int);
    int (*test)(struct __joe6simpleE, int);
};
struct __joe6simpleE {
    struct __joe6simple4dataE* data;
    struct __joe6simple6vtableE* vtable;
};

void __joe6simple6simpleE(struct __joe6simpleE self, int __joe6simple6simple1nE) {
    (((self).data)->_n) = (__joe6simple6simple1nE);
}

int __joe6simple4testE(struct __joe6simpleE self, int __joe6simple4test1aE) {
    return (__joe6simple4test1aE) + (((self).data)->_n);
}

struct __joe6simple6vtableE __joe6simple6vtableE = {__joe6simple6simpleE, __joe6simple4testE};

void __joe6simple4mainE() {
    struct __joe6simpleE __joe_local_0;
    printf("%d\n", (((((__joe_local_0).data) = (malloc(sizeof(struct __joe6simple4dataE))), ((__joe_local_0).vtable) = (&(__joe6simple6vtableE)), ((__joe_local_0).vtable)->simple(__joe_local_0, 123), __joe_local_0)).vtable)->test(__joe_local_0, 234));
}

int main() {
    __joe6simple4mainE();
    return 0;
}
/* end module simple */
```

It's not pretty, but it does compile, and it sure does print `357\n` :sunglasses:
