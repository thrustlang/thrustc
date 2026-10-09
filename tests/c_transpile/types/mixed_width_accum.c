#include <stdbool.h>
#include <stdint.h>
typedef long long i64;
typedef unsigned long long u64;
typedef unsigned short u16;
typedef signed char i8;
static u64 fold_u32(const unsigned int *vals, int n) {
    u64 acc = 0;
    for (int i = 0; i < n; i++) {
        acc = acc + (u64)vals[i];
    }
    return acc;
}
static i64 fold_i32(const int *vals, int n) {
    i64 acc = 0;
    for (int i = 0; i < n; i++) {
        acc += (i64)vals[i];
    }
    return acc;
}
static int average_floor(const int *vals, int n) {
    if (n <= 0) return 0;
    i64 total = fold_i32(vals, n);
    return (int)(total / (i64)n);
}
static u16 wrap16(unsigned int value) {
    u16 low = (u16)value;
    return (u16)(low + 1u);
}
static bool fits_i8(int value) { return value >= -128 && value <= 127; }
int mixed_width_accum_check(void) {
    int signed_vals[5] = {10, -20, 30, -40, 50};
    unsigned int unsigned_vals[5] = {1u, 2u, 3u, 4u, 5u};
    if (fold_i32(signed_vals, 5) != 30) return 1;
    if (fold_u32(unsigned_vals, 5) != 15u) return 2;
    if (average_floor(signed_vals, 5) != 6) return 3;
    i64 wide = (i64)fold_u32(unsigned_vals, 5) * 1000000LL;
    if (wide != 15000000LL) return 4;
    unsigned int big = 4294967295u;
    if (wrap16(big) != 0u) return 5;
    i8 small = (i8)(-100);
    int promoted = small;
    if (promoted != -100) return 6;
    if (!fits_i8(promoted)) return 7;
    short s = (short)(70000);
    int widened = s;
    if (widened == 70000) return 8;
    long l = 100000L;
    int narrowed = (int)(l + 5L);
    if (narrowed != 100005) return 9;
    return 0;
}
