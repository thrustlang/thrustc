int printf(const char *fmt, ...);

static int marker(int value) {
    return value + 2;
}

static int finish(int value) {
    return value + 3;
}

#define VAR_LOG_EMPTY(fmt, ...) \
    printf((const char *)(fmt), ##__VA_ARGS__)

int ext_varargs_comma_empty_value(void) {
    int i;
    int total = 0;

    for (i = 0; i < 3; i++) {
        VAR_LOG_EMPTY("ok\n");
        total = total + marker(i);
    }

    VAR_LOG_EMPTY("done\n");
    return finish(total + 17);
}
