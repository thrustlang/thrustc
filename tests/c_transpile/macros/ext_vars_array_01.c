#define ARRAY_1(out) do { \
    int values[4] = {1, 2, 3, 4}; \
    int matrix[2][2] = {{1, 2}, {3, 4}}; \
    (out) = values[0] + values[3] + matrix[1][1]; \
} while (0)

int main(void) {
    int out = 0;
    ARRAY_1(out);
    return out == 9 ? 0 : 1;
}
