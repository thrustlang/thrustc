struct Pair5 { int left; int right; };

#define PAIR_5(out) do { \
    struct Pair5 pair = { .right = 5, .left = 12 }; \
    int extra = 8; \
    (out) = pair.left * 10 + pair.right + extra; \
} while (0)

int main(void) {
    int out = 0;
    PAIR_5(out);
    return out == 133 ? 0 : 1;
}
