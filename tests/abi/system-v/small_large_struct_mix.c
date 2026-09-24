#include <stdint.h>

struct Small {
    uint16_t a;
    uint16_t b;
};

struct Large {
    uint64_t x;
    uint64_t y;
    uint64_t z;
};

int32_t combine(struct Small s, struct Large l) {
    if (s.a != 3) return 1;
    if (s.b != 4) return 2;
    if (l.x != 5) return 3;
    if (l.y != 6) return 4;
    if (l.z != 7) return 5;
    return 0;
}