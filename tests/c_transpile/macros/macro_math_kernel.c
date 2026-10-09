#define BITS 16
#define MASK ((1 << BITS) - 1)
#define XOR_SWAP(a, b) \
    do { \
        (a) = (a) ^ (b); \
        (b) = (a) ^ (b); \
        (a) = (a) ^ (b); \
    } while (0)
#define ABSV(x) ((x) < 0 ? -(x) : (x))
#define GCD_LOOP(u, v) \
    do { \
        while ((v) != 0) { \
            int t = (u) % (v); \
            (u) = (v); \
            (v) = (t); \
        } \
    } while (0)
#define LCG_NEXT(s) (((s) * 1103515245 + 12345) & MASK)
#if defined(MASK) && BITS == 16
#define KERNEL_OK 1
#else
#define KERNEL_OK 0
#endif

static int gcd(int u, int v) {
    GCD_LOOP(u, v);
    return u;
}

static int lcg_advance(int seed, int steps) {
    int state = seed;
    for (int i = 0; i < steps; i++) {
        state = LCG_NEXT(state);
    }
    return state;
}

int macro_math_kernel_check(void) {
    int x = 12;
    int y = 30;
    int state;
    if (KERNEL_OK != 1) { return 1; }
    if (gcd(x, y) != 6) { return 2; }
    XOR_SWAP(x, y);
    if (x != 30 || y != 12) { return 3; }
    if (ABSV(-7) != 7 || ABSV(5) != 5) { return 4; }
    state = lcg_advance(12345, 3);
    if (state < 0 || state > MASK) { return 5; }
    return 0;
}
