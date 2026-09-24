#include <stdint.h>

struct Triplet {
    uint8_t a;
    uint8_t b;
    uint8_t c;
};

struct Triplet make_triplet(void) {
    struct Triplet t;
    t.a = 5;
    t.b = 6;
    t.c = 7;
    return t;
}