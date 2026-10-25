static int bump(int value) {
    return value + 1;
}

#define VAR_FIRST(first, ...) \
    (first)

#define VAR_KEEP(first, ...) \
    (bump(first))

int ext_varargs_zero_value(void) {
    int total = 0;
    int i;

    total = total + VAR_FIRST(9);
    total = total + VAR_FIRST(4, 1, 2);
    total = total + VAR_KEEP(10);
    total = total + VAR_KEEP(20, 7, 8, 9);

    for (i = 0; i < 4; i++) {
        total = total + VAR_FIRST(i, i + 100);
    }

    return total;
}
