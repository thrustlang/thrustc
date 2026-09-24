#include <stdint.h>

int8_t r_s8(int8_t value) {
    return value;
}

uint16_t r_u16(uint16_t value) {
    return value;
}

int8_t call_s8(int8_t value) {
    return r_s8(value);
}

uint16_t call_u16(uint16_t value) {
    return r_u16(value);
}