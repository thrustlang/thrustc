#define VOLATILE_SET(dst, value) do { volatile int local = (value); (dst) = local; } while (0)

int volatile_declarations(void) {
    int value = 0;
    VOLATILE_SET(value, 42);
    return value;
}
