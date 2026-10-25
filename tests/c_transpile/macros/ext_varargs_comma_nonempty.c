int printf(const char *fmt, ...);

static int weight(int value) {
    return value * 2;
}

#define VAR_LOG_FILLED(fmt, ...) \
    printf((const char *)(fmt), ## __VA_ARGS__)

int ext_varargs_comma_nonempty_value(void) {
    int total = 0;
    int i;

    for (i = 1; i <= 4; i++) {
        VAR_LOG_FILLED("%d\n", weight(i));
        total = total + i;
    }

    VAR_LOG_FILLED("%s %d\n", "sum", total);

    for (i = 0; i < 3; i++) {
        VAR_LOG_FILLED("%d\n", weight(i + 1));
        total = total + 1;
    }

    return total + 10;
}
