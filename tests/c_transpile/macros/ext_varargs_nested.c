#define INNER(a, b) \
    ((a) + (b))

#define INNER3(a, b, c) \
    ((a) + (b) + (c))

#define WRAP(...) \
    INNER(__VA_ARGS__)

#define WRAP3(...) \
    INNER3(__VA_ARGS__)

#define OUTER(first, ...) \
    WRAP((first), (__VA_ARGS__))

#define STACK(a, ...) \
    WRAP3((a), __VA_ARGS__)

int ext_varargs_nested_value(void) {
    int total = 0;
    int i;

    total = total + WRAP(6, 7);
    total = total + OUTER(1, 2);
    total = total + WRAP3(1, 2, 3);
    total = total + STACK(4, 5, 6);

    for (i = 0; i < 3; i++) {
        total = total + OUTER(i, i + 1);
    }

    return total;
}
