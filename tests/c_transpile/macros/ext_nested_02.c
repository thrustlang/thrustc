#define DOUBLE_2(x) ((x) * 2)
#define APPLY_2(x, out) do { \
    (out) = DOUBLE_2((x)) + 3; \
} while (0)
#define COMPOSE_2(x, out) do { \
    int value = (x); \
    int temp = 0; \
    APPLY_2(value, temp); \
    APPLY_2(temp, out); \
} while (0)

int main(void) {
    int out = 0;
    COMPOSE_2(2, out);
    int value = 2;
    int temp = value * 2 + 3;
    int expected = temp * 2 + 3;
    return out == expected ? 0 : 1;
}
