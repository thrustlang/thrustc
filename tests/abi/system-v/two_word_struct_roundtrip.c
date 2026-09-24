#include <stdint.h>

struct Pair64 {
    uint64_t first;
    uint64_t second;
};

int32_t check_pair64(struct Pair64 value) {
    if (value.first != 7) return 1;
    if (value.second != 9) return 2;
    return 0;
}