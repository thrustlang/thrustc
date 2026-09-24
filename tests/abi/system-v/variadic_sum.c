#include <stdarg.h>
#include <stdint.h>

int32_t sum(int32_t count, ...) {
    va_list ap;
    int32_t total = 0;
    int32_t i;
    va_start(ap, count);
    for (i = 0; i < count; i++) {
        total += va_arg(ap, int32_t);
    }
    va_end(ap);
    return total;
}