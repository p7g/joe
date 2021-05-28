class simple {
    int _n;

    simple(int n) {
        _n = n;
    }

    int test(int a) {
        return a + _n;
    }

    static void main() {
        println(new simple(123).test(234));
    }
}
