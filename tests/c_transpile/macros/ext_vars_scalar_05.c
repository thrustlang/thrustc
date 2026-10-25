#define COMPUTE_5(out) do { \
    int first = 14, second = 15; \
    const int bias = 16; \
    int total = first + second * 2 + bias; \
    (out) = total; \
} while (0)

int main(void) {
    int out = 0;
    COMPUTE_5(out);
    return out == 60 ? 0 : 1;
}
