#define COMPUTE_1(out) do { \
    int first = 2, second = 3; \
    const int bias = 4; \
    int total = first + second * 2 + bias; \
    (out) = total; \
} while (0)

int main(void) {
    int out = 0;
    COMPUTE_1(out);
    return out == 12 ? 0 : 1;
}
