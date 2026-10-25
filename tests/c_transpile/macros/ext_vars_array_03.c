#define ARRAY_3(out) do { \
    int values[4] = {3, 3, 3, 3}; \
    int matrix[2][2] = {{3, 3}, {3, 3}}; \
    (out) = values[0] + values[3] + matrix[1][1]; \
} while (0)

int main(void) {
    int out = 0;
    ARRAY_3(out);
    return out == 9 ? 0 : 1;
}
