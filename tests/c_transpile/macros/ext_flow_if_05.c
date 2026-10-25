#define SELECT_5(a, b, out) do { \
    int x = (a); \
    int y = (b); \
    int z = 9; \
    if (x > y && z > 0) { \
        (out) = x * 10 + z; \
    } else if (x == y) { \
        (out) = x + y + z; \
    } else { \
        (out) = y - x + z; \
    } \
} while (0)

int main(void) {
    int out = 0;
    SELECT_5(2, 6, out);
    int expected = (2 > 6 && 9 > 0) ? (2 * 10 + 9) : ((2 == 6) ? (2 + 6 + 9) : (6 - 2 + 9));
    return out == expected ? 0 : 1;
}
