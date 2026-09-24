#include <stdint.h>

struct Mixed {
    uint8_t a;
    uint16_t b;
    uint32_t c;
};

int32_t check_mixed(struct Mixed value) {
    if (value.a != 1) return 1;
    if (value.b != 2) return 2;
    if (value.c != 3) return 3;
    return 0;
}