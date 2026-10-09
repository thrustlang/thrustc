#define WINDOW 4
#define BIAS 3
#define LIMIT 1000
#define SQUARE(x) ((x) * (x))
#define TRIPLE(x) ((x) + (x) + (x))
#define BLEND(a, b) (TRIPLE(a) + SQUARE(b))
#define GROW(acc, v) \
    do { \
        (acc) = (acc) + BLEND((v), WINDOW); \
    } while (0)
#if WINDOW > 2
#define TIGHT 1
#elif WINDOW == 2
#define TIGHT 0
#else
#define TIGHT -1
#endif
#ifndef LIMIT
#define LIMIT 0
#endif

static int fold(const int *src, int n) {
    int acc = BIAS;
    for (int i = 0; i < n; i++) {
        if (src[i] >= 0) {
            GROW(acc, src[i]);
        } else {
            acc = acc - BIAS;
        }
    }
    return acc;
}

static int run_window(const int *src) {
    int acc = 0;
    for (int i = 0; i < WINDOW; i++) {
        int local = src[i];
        while (local > 0 && acc < LIMIT) {
            acc = acc + BLEND(1, local);
            local = local - 1;
        }
    }
    return acc;
}

int macro_accumulate_loop_check(void) {
    int src[WINDOW] = { 2, -1, 3, 0 };
    int acc = BIAS;
    if (TIGHT != 1) { return 1; }
    for (int i = 0; i < WINDOW; i++) { acc = acc + SQUARE(src[i]); }
    if (acc != BIAS + 4 + 1 + 9 + 0) { return 2; }
    if (fold(src, WINDOW) <= 0) { return 3; }
    if (run_window(src) <= 0) { return 4; }
    return 0;
}
