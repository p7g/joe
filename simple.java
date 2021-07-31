class simple {
    int _n;

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
