#define DOUBLE_1(x) ((x) * 2)
#define APPLY_1(x, out) do { \
    (out) = DOUBLE_1((x)) + 1; \
} while (0)
#define COMPOSE_1(x, out) do { \
    int value = (x); \
    int temp = 0; \
    APPLY_1(value, temp); \
    APPLY_1(temp, out); \
} while (0)

int main(void) {
    int out = 0;
    COMPOSE_1(1, out);
    int value = 1;
    int temp = value * 2 + 1;
    int expected = temp * 2 + 1;
    return out == expected ? 0 : 1;
}
