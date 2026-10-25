#define SELECT_1(a, b, out) do { \
    int x = (a); \
    int y = (b); \
    int z = 3; \
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
    SELECT_1(1, 2, out);
    int expected = (1 > 2 && 3 > 0) ? (1 * 10 + 3) : ((1 == 2) ? (1 + 2 + 3) : (2 - 1 + 3));
    return out == expected ? 0 : 1;
}
