#define SELECT_4(a, b, out) do { \
    int x = (a); \
    int y = (b); \
    int z = 6; \
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
    SELECT_4(9, 3, out);
    int expected = (9 > 3 && 6 > 0) ? (9 * 10 + 6) : ((9 == 3) ? (9 + 3 + 6) : (3 - 9 + 6));
    return out == expected ? 0 : 1;
}
