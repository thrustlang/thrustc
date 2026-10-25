int printf(const char *fmt, ...);

static int scale(int value) {
    return value * 2;
}

static int finish(int value) {
    return value + 1;
}

#define VAR_LOG_NAMED_TAIL(fmt, args...) \
    printf((const char *)(fmt), ## args, 0)

int ext_varargs_comma_named_tail_value(void) {
    int i;
    int total = 0;

    for (i = 0; i < 3; i++) {
        VAR_LOG_NAMED_TAIL("%d\n", scale(i));
        total = total + scale(i);
    }

    VAR_LOG_NAMED_TAIL("tail\n");
    return finish(19 + total);
}
