struct Pair4 { int left; int right; };

#define PAIR_4(out) do { \
    struct Pair4 pair = { .right = 9, .left = 4 }; \
    int extra = 3; \
    (out) = pair.left * 10 + pair.right + extra; \
} while (0)

int main(void) {
    int out = 0;
    PAIR_4(out);
    return out == 52 ? 0 : 1;
}
