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
#include <stdlib.h>
struct __joe_simple {
    struct __joe_simple___0data* data;
    struct __joe_simple___0vtable* vtable;
};
struct __joe_simple___0data {
    int _n;
};
struct __joe_simple___0vtable {
    void (*simple)(struct __joe_simple, int);
    int (*test)(struct __joe_simple, int);
};
void __joe_simple___simple(struct __joe_simple self, int __joe_n);
int __joe_simple___test(struct __joe_simple self, int __joe_a);
void __joe_simple___main();
void __joe_simple___println(int __joe_n);
int main();
struct __joe_simple___0vtable __joe_simple___0vtable = {__joe_simple___simple, __joe_simple___test};
void __joe_simple___simple(struct __joe_simple self, int __joe_n) {
    (((self).data)->_n) = (__joe_n);
}
int __joe_simple___test(struct __joe_simple self, int __joe_a) {
    return (__joe_a) + (((self).data)->_n);
}
void __joe_simple___main() {
    struct __joe_simple __joe_local_0;
    int __joe_local_1;
    ((__joe_local_0).data) = (malloc(sizeof(struct __joe_simple___0data)));
    ((__joe_local_0).vtable) = (&(__joe_simple___0vtable));
    ((__joe_local_0).vtable)->simple(__joe_local_0, 123);
    (__joe_local_1) = (((__joe_local_0).vtable)->test(__joe_local_0, 234));
    __joe_simple___println(__joe_local_1);
}
void __joe_simple___println(int __joe_n) {
}
int main() {
    __joe_simple___main();
    return 0;
}
```

It's not pretty, but it does compile, and it ~sure does print `357\n`~ doesn't
print anything because I need to figure out C interop.
