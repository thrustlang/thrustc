#define SELECT_3(a, b, out) do { \
    int x = (a); \
    int y = (b); \
    int z = 2; \
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
    SELECT_3(7, 8, out);
    int expected = (7 > 8 && 2 > 0) ? (7 * 10 + 2) : ((7 == 8) ? (7 + 8 + 2) : (8 - 7 + 2));
    return out == expected ? 0 : 1;
}
