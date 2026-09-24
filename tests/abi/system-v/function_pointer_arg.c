#include <stdint.h>

typedef int32_t (*callback_t)(int32_t);

int32_t double_it(int32_t value) {
    return value * 2;
}

int32_t apply(int32_t value, callback_t cb) {
    return cb(value);
}