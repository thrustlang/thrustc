#include <stdint.h>

struct Wide {
    uint64_t a;
    uint64_t b;
    uint64_t c;
    uint64_t d;
};

int32_t check_wide(struct Wide value) {
    if (value.a != 1) return 1;
    if (value.b != 2) return 2;
    if (value.c != 3) return 3;
    if (value.d != 4) return 4;
    return 0;
}