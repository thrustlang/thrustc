#define EXPR_5(a, b, out) do { \
    int x = (a); \
    int y = (b); \
    int mixed = (x > y ? x * 2 : y * 3); \
    int bits = ((x << 2) | (y & 3)) ^ 5; \
    (out) = mixed + bits + (int)sizeof(x); \
} while (0)

int main(void) {
    int out = 0;
    EXPR_5(6, 6, out);
    int mixed = (6 > 6 ? 6 * 2 : 6 * 3);
    int bits = ((6 << 2) | (6 & 3)) ^ 5;
    return out == mixed + bits + 4 ? 0 : 1;
}
