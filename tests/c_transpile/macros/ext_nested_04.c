#define DOUBLE_4(x) ((x) * 2)
#define APPLY_4(x, out) do { \
    (out) = DOUBLE_4((x)) + 4; \
} while (0)
#define COMPOSE_4(x, out) do { \
    int value = (x); \
    int temp = 0; \
    APPLY_4(value, temp); \
    APPLY_4(temp, out); \
} while (0)

int main(void) {
    int out = 0;
    COMPOSE_4(7, out);
    int value = 7;
    int temp = value * 2 + 4;
    int expected = temp * 2 + 4;
    return out == expected ? 0 : 1;
}
