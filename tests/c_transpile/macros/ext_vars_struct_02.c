struct Pair2 { int left; int right; };

#define PAIR_2(out) do { \
    struct Pair2 pair = { .right = 2, .left = 8 }; \
    int extra = 6; \
    (out) = pair.left * 10 + pair.right + extra; \
} while (0)

int main(void) {
    int out = 0;
    PAIR_2(out);
    return out == 88 ? 0 : 1;
}
