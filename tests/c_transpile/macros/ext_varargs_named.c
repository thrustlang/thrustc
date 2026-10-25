static int offset(int value) {
    return value + 2;
}

#define VAR_NAMED(first, rest...) \
    ((first) - (rest))

#define VAR_NAMED_ADD(first, rest...) \
    ((first) + (rest))

int ext_varargs_named_value(void) {
    int total = 0;
    int i;

    total = total + VAR_NAMED(9, 4);
    total = total + VAR_NAMED(20, 3);

    for (i = 0; i < 4; i++) {
        total = total + VAR_NAMED_ADD(i, i + 1);
    }

    for (i = 0; i < 5; i++) {
        total = total + VAR_NAMED_ADD(offset(i), i + 1);
    }

    return total;
}
