#define ARRAY_LEN 6
#define SENTINEL (-1)
#define AT(p, i) ((p)[(i)])
#define ADVANCE(p, n) ((p) + (n))
#define SET_IF(cond, dst, val) \
    do { \
        if (cond) { \
            (dst) = (val); \
        } \
    } while (0)
#define MAX2(a, b) ((a) > (b) ? (a) : (b))
#define MIN2(a, b) ((a) < (b) ? (a) : (b))
#define CLAMP(v, lo, hi) MAX2((lo), MIN2((v), (hi)))
#define FIRST(p) AT(p, 0)
#define LAST(p, n) AT(p, (n) - 1)
#ifdef ARRAY_LEN
#define HAS_LEN 1
#else
#define HAS_LEN 0
#endif

static int sum_ptr(const int *p, int n) {
    int s = 0;
    for (int i = 0; i < n; i++) {
        s = s + *(ADVANCE(p, i));
    }
    return s;
}

static int spread_ptr(const int *p, int n) {
    int hi = FIRST(p);
    int lo = FIRST(p);
    for (int i = 1; i < n; i++) {
        hi = MAX2(hi, AT(p, i));
        lo = MIN2(lo, AT(p, i));
    }
    return hi - lo;
}
int macro_pointer_scan_check(void) {
    int values[ARRAY_LEN] = { 4, -2, 9, 0, 7, 3 };
    int *scan = values;
    int total;
    int clamped;
    if (HAS_LEN != 1) {
        return 1;
    }
    SET_IF(LAST(values, ARRAY_LEN) != SENTINEL, values[ARRAY_LEN - 1], 5);
    total = sum_ptr(scan, ARRAY_LEN);
    if (total != 23) {
        return 2;
    }
    clamped = CLAMP(total, 0, 20);
    if (clamped != 20) {
        return 3;
    }
    if (spread_ptr(ADVANCE(scan, 1), ARRAY_LEN - 1) != 11) {
        return 4;
    }
    return 0;
}
