struct Pair1 { int left; int right; };

#define PAIR_1(out) do { \
    struct Pair1 pair = { .right = 4, .left = 3 }; \
    int extra = 5; \
    (out) = pair.left * 10 + pair.right + extra; \
} while (0)

int main(void) {
    int out = 0;
    PAIR_1(out);
    return out == 39 ? 0 : 1;
}
