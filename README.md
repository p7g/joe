The aim is a language that satisfies the following criteria:
- Compiles to decent native code with minimal runtime
- Can build abstractions with reasonably-low cost
- Automatic memory management
- Feels familiar and comfy (if you're familiar with C-like languages)
- Simple nominal type system with generics
- Few constraints on code organization (multiple classes in a file pls)
- Predictable performance and behaviour
- Easy dynamic dispatch

Preview:

```java
interface Xyz<T, U> extends Zyx<T> {
    U somemeth(T arg);

    T hello() {

    }
}

interface Zyx<T> {}

class Abc<T: String> implements Xyz<T, Integer> {
    String yo;

    Abc() {
    }

    void method(Integer a) {
    }
}
```

Goals:
- Simple but "legit" in language design and implementation
- No class inheritance; just interfaces
- Interface method default implementation
    - Like a method where "this" is generic, constrained by the interface
- No boxing
    - A class with a single "final" field requires no heap allocation
    - Objects have no intrinsic identity
- Refcount GC
    - Can opt out as a type modifier (have yet to think this through)
- No dependencies
- Some optimization

Compiler plan:
1. Parsing
2. Type checking/semantic analysis
3. Lower to hybrid IR (CFG + linear SSA), similar to LLVM
4. Optimize somewhat
5. Lower to machine code
