#define ARRAY_2(out) do { \
    int values[4] = {5, 1, 7, 2}; \
    int matrix[2][2] = {{5, 1}, {7, 2}}; \
    (out) = values[0] + values[3] + matrix[1][1]; \
} while (0)

int main(void) {
    int out = 0;
    ARRAY_2(out);
    return out == 9 ? 0 : 1;
}
