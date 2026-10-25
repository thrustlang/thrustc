int printf(const char *fmt, ...);

#define VAR_TRACE(fmt, ...) \
    do { \
        printf((const char *)(fmt), ##__VA_ARGS__); \
    } while (0)

int ext_varargs_statement_comma_value(void) {
    int i;
    int checksum = 0;

    VAR_TRACE("start\n");

    for (i = 0; i < 4; i++) {
        VAR_TRACE("%d\n", i * 2);
        checksum = checksum + i;
    }

    for (i = 1; i <= 3; i++) {
        VAR_TRACE("%d\n", i * 10);
        checksum = checksum + i;
    }

    VAR_TRACE("end\n");
    return 21 + checksum;
}
