#include <stdint.h>
#include <stdbool.h>

typedef int64_t wide_t;
typedef uint32_t uint_t;
typedef int16_t small_t;
typedef uint8_t nibble_t;

static small_t to_small(wide_t value) { return (small_t)value; }
static uint_t to_uint(wide_t value) { return (uint_t)value; }

static wide_t scale(wide_t value, uint_t factor) {
    return value * (wide_t)factor;
}

static int saturate_cast(wide_t value, int lo, int hi) {
    if (value < (wide_t)lo) return lo;
    if (value > (wide_t)hi) return hi;
    return (int)value;
}

static nibble_t low_nibble(uint_t value) { return (nibble_t)(value & 0x0Fu); }

static bool same_bits_u16(uint_t value, small_t other) {
    uint16_t a = (uint16_t)value;
    uint16_t b = (uint16_t)other;
    return a == b;
}

int cast_roundtrip_check(void) {
    wide_t w = 70000;
    small_t s = to_small(w);
    int narrowed = s;
    if (narrowed == 70000) return 1;

    if (to_uint((wide_t)-1) != 0xFFFFFFFFu) return 2;

    if (scale(3, 4u) != 12) return 3;
    if (scale((wide_t)-2, 5u) != -10) return 4;

    if (saturate_cast(1000, 0, 255) != 255) return 5;
    if (saturate_cast((wide_t)-50, 0, 255) != 0) return 6;
    if (saturate_cast(42, 0, 255) != 42) return 7;

    if (low_nibble(0xABCu) != 0xCu) return 8;

    if (!same_bits_u16(0x1FFFFu, (small_t)-1)) return 9;

    wide_t mixed = (wide_t)5 + 2.5;
    if (mixed != 7) return 10;

    double back = (double)(uint_t)4000000000u;
    if (back < 3999999999.0) return 11;

    return 0;
}
