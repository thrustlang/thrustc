#define ADDRESS_TEST(out) do { \
    int matrix[2][3] = {{1, 2, 3}, {4, 5, 6}}; \
    int *cell = &matrix[0][1]; \
    *cell = 9; \
    (out) = matrix[0][1] * 100 + matrix[1][0] + matrix[1][1] + matrix[1][2]; \
} while (0)

int main(void) {
    int value = 0;
    ADDRESS_TEST(value);
    if (value != 915) return 1;
    return 0;
}
