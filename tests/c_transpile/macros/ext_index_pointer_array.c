#define STORE_ROWS(out) do { \
    int first[2] = {0, 0}; \
    int second[2] = {0, 0}; \
    int *rows[2] = {(int *)first, (int *)second}; \
    rows[0][0] = 5; \
    rows[1][1] = 6; \
    (out) = first[0] * 10 + second[1]; \
} while (0)

#define STORE_MATRIX(out) do { \
    int matrix[2][2] = {{0, 0}, {0, 0}}; \
    matrix[0][1] = 7; \
    matrix[1][0] = 9; \
    (out) = matrix[0][1] * 10 + matrix[1][0]; \
} while (0)

int main(void) {
    int rows_result = 0;
    int matrix_result = 0;
    STORE_ROWS(rows_result);
    STORE_MATRIX(matrix_result);
    if (rows_result != 56) return 1;
    if (matrix_result != 79) return 2;
    return 0;
}
