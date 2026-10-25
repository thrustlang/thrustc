#define ARRAY_5(out) do { \
    int values[4] = {2, 8, 5, 6}; \
    int matrix[2][2] = {{2, 8}, {5, 6}}; \
    (out) = values[0] + values[3] + matrix[1][1]; \
} while (0)

int main(void) {
    int out = 0;
    ARRAY_5(out);
    return out == 14 ? 0 : 1;
}
