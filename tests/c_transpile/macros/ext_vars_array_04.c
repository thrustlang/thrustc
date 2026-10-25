#define ARRAY_4(out) do { \
    int values[4] = {9, 6, 4, 1}; \
    int matrix[2][2] = {{9, 6}, {4, 1}}; \
    (out) = values[0] + values[3] + matrix[1][1]; \
} while (0)

int main(void) {
    int out = 0;
    ARRAY_4(out);
    return out == 11 ? 0 : 1;
}
