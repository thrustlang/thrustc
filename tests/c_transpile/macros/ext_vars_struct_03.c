struct Pair3 { int left; int right; };

#define PAIR_3(out) do { \
    struct Pair3 pair = { .right = 7, .left = 10 }; \
    int extra = 1; \
    (out) = pair.left * 10 + pair.right + extra; \
} while (0)

int main(void) {
    int out = 0;
    PAIR_3(out);
    return out == 108 ? 0 : 1;
}
