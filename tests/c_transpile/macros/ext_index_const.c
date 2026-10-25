#define READ_CONST(out) do { \
    const int matrix[2][2] = {{2, 3}, {4, 5}}; \
    (out) = matrix[1][0]; \
} while (0)

#define SUM_CONST(out) do { \
    const int matrix[2][2] = {{2, 3}, {4, 5}}; \
    (out) = matrix[0][0] + matrix[0][1] + matrix[1][0] + matrix[1][1]; \
} while (0)

int main(void) {
    int value = 0;
    int total = 0;
    READ_CONST(value);
    SUM_CONST(total);
    if (value != 4) return 1;
    if (total != 14) return 2;
    return 0;
}
