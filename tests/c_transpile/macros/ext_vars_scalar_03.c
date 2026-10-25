#define COMPUTE_3(out) do { \
    int first = 8, second = 9; \
    const int bias = 10; \
    int total = first + second * 2 + bias; \
    (out) = total; \
} while (0)

int main(void) {
    int out = 0;
    COMPUTE_3(out);
    return out == 36 ? 0 : 1;
}
