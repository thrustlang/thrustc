#define SELECT_2(a, b, out) do { \
    int x = (a); \
    int y = (b); \
    int z = 4; \
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
    SELECT_2(5, 1, out);
    int expected = (5 > 1 && 4 > 0) ? (5 * 10 + 4) : ((5 == 1) ? (5 + 1 + 4) : (1 - 5 + 4));
    return out == expected ? 0 : 1;
}
