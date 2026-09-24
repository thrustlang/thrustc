#include <stdint.h>

double pack(float a, int32_t b, double c, uint64_t d) {
    return (double)a + (double)b + c + (double)d;
}