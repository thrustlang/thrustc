#include <stdint.h>

struct Wide {
    uint64_t a;
    uint64_t b;
    uint64_t c;
    uint64_t d;
};

struct Wide make_wide(void) {
    struct Wide w;
    w.a = 8;
    w.b = 9;
    w.c = 10;
    w.d = 11;
    return w;
}