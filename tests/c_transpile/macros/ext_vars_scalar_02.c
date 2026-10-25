#define COMPUTE_2(out) do { \
    int first = 5, second = 6; \
    const int bias = 7; \
    int total = first + second * 2 + bias; \
    (out) = total; \
} while (0)

int main(void) {
    int out = 0;
    COMPUTE_2(out);
    return out == 24 ? 0 : 1;
}
