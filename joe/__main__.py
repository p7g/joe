from joe.parse import parse, scan

text = """
interface Zyx<T> {}

interface Xyz<T, U> extends Zyx<T> {
    U somemeth(T arg);

    T hello() {

    }
}

class Abc<T: String> implements Xyz<T, int> {
    String yo;

    Abc() {
    }

    void method(int a) {
    }
}
"""

for node in parse(scan("<script>", text)):
    print(repr(node))
