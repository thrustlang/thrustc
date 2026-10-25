#define EXPR_2(a, b, out) do { \
    int x = (a); \
    int y = (b); \
    int mixed = (x > y ? x * 2 : y * 3); \
    int bits = ((x << 2) | (y & 3)) ^ 5; \
    (out) = mixed + bits + (int)sizeof(x); \
} while (0)

int main(void) {
    int out = 0;
    EXPR_2(7, 2, out);
    int mixed = (7 > 2 ? 7 * 2 : 2 * 3);
    int bits = ((7 << 2) | (2 & 3)) ^ 5;
    return out == mixed + bits + 4 ? 0 : 1;
}
