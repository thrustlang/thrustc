#include <stdint.h>
#include <stdbool.h>

struct Mixed {
    uint32_t a;
    double b;
    bool c;
};

struct Mixed identity(struct Mixed value) {
    return value;
}

void consume(struct Mixed value) {
    (void)value;
}