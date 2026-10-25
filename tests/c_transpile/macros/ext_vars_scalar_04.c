#define COMPUTE_4(out) do { \
    int first = 11, second = 12; \
    const int bias = 13; \
    int total = first + second * 2 + bias; \
    (out) = total; \
} while (0)

int main(void) {
    int out = 0;
    COMPUTE_4(out);
    return out == 48 ? 0 : 1;
}
