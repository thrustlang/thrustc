static int scale(int value) {
    return value * 2;
}

#define VAR_OPT(first, ...) \
    ((first) __VA_OPT__(+ (__VA_ARGS__)))

#define VAR_OPT_MUL(first, ...) \
    ((first) __VA_OPT__(* (__VA_ARGS__)))

int ext_varargs_va_opt_value(void) {
    int total = 0;
    int i;

    total = total + VAR_OPT(3, 4);
    total = total + VAR_OPT(3);
    total = total + VAR_OPT_MUL(2, 5);
    total = total + VAR_OPT_MUL(7);

    for (i = 1; i <= 3; i++) {
        total = total + VAR_OPT(i, i + 1);
    }

    total = total + VAR_OPT_MUL(scale(1), scale(2));
    return total;
}
