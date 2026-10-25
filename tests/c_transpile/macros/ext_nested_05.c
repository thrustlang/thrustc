#define DOUBLE_5(x) ((x) * 2)
#define APPLY_5(x, out) do { \
    (out) = DOUBLE_5((x)) + 5; \
} while (0)
#define COMPOSE_5(x, out) do { \
    int value = (x); \
    int temp = 0; \
    APPLY_5(value, temp); \
    APPLY_5(temp, out); \
} while (0)

int main(void) {
    int out = 0;
    COMPOSE_5(5, out);
    int value = 5;
    int temp = value * 2 + 5;
    int expected = temp * 2 + 5;
    return out == expected ? 0 : 1;
}
