#include <stdint.h>

struct Inner {
    uint64_t x;
    uint64_t y;
};

struct Outer {
    struct Inner inner;
    uint64_t z;
};

int32_t check_outer(struct Outer value) {
    if (value.inner.x != 1) return 1;
    if (value.inner.y != 2) return 2;
    if (value.z != 3) return 3;
    return 0;
}