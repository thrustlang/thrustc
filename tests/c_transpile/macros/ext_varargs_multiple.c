static int add(int a, int b) {
    return a + b;
}

static int add3(int a, int b, int c) {
    return a + b + c;
}

static int add4(int a, int b, int c, int d) {
    return a + b + c + d;
}

#define VAR_CALL(fn, ...) \
    fn( \
        __VA_ARGS__ \
    )

int ext_varargs_multiple_value(void) {
    int total = 0;
    int i;

    total = total + VAR_CALL(add, 4, 5);
    total = total + VAR_CALL(add3, 1, 2, 3);
    total = total + VAR_CALL(add4, 1, 2, 3, 4);

    for (i = 0; i < 3; i++) {
        total = total + VAR_CALL(add, i, i + 10);
    }

    return total;
}
