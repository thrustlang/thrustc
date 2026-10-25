#define NESTED_WRITE(out) do { \
    int matrix[2][3] = {{0, 0, 0}, {0, 0, 0}}; \
    matrix[0][1] = 4; \
    matrix[1][2] = 9; \
    matrix[0][1] += 6; \
    int *cell = &matrix[0][1]; \
    (out) = *cell * 100 + matrix[1][2]; \
} while (0)

int main(void) {
    int value = 0;
    NESTED_WRITE(value);
    if (value != 1009) return 1;
    return 0;
}
