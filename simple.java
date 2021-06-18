class simple {
    int _n;

    void simple(int n) {
        _n = n;
    }

    int test(int a) {
        return a + _n;
    }

    static void main() {
        println(new simple(123).test(234));
    }

    static void println(int n) {}
}
