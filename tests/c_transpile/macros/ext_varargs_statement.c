static int clamp_40(int value) {
    if (value > 40) {
        return 40;
    }

    return value;
}

#define VAR_APPLY(target, scale, ...) \
    do { \
        int step = (__VA_ARGS__); \
        (target)[0] = (target)[0] * (scale) + step; \
        if ((target)[0] > 40) { \
            (target)[0] = 40; \
        } \
        (target)[0] = clamp_40((target)[0] - step); \
    } while (0)

int ext_varargs_statement_value(void) {
    int values[3] = {7, 4, 12};
    int i;

    for (i = 0; i < 3; i++) {
        VAR_APPLY(&values[i], 5, i + 2);
    }

    return values[0] * 100 + values[1] * 10 + values[2];
}
