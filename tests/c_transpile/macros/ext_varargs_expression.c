static int seed(int value) {
    return value * 2 + 1;
}

static int adjust(int value) {
    return value - 1;
}

#define VAR_SUM(first, ...) \
    ((first) + \
     (__VA_ARGS__))

int ext_varargs_expression_value(void) {
    int total = 0;
    int i;

    for (i = 0; i < 5; i++) {
        int value = seed(i);
        total = total + VAR_SUM(value, i + 1);
    }

    for (i = 1; i <= 3; i++) {
        total = total + VAR_SUM(adjust(i), i * 2);
    }

    return total;
}
