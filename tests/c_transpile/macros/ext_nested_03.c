#define DOUBLE_3(x) ((x) * 2)
#define APPLY_3(x, out) do { \
    (out) = DOUBLE_3((x)) + 2; \
} while (0)
#define COMPOSE_3(x, out) do { \
    int value = (x); \
    int temp = 0; \
    APPLY_3(value, temp); \
    APPLY_3(temp, out); \
} while (0)

int main(void) {
    int out = 0;
    COMPOSE_3(4, out);
    int value = 4;
    int temp = value * 2 + 2;
    int expected = temp * 2 + 2;
    return out == expected ? 0 : 1;
}
