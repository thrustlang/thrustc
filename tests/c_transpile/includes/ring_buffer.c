#include <stddef.h>
#include "ring_buffer.h"

static int clamp_slot(int value, int limit) {
    if (value < 0) {
        return 0;
    }
    if (value >= limit) {
        return limit - 1;
    }
    return value;
}

int ring_fill(int seed, size_t count) {
    int total = 0;
    size_t i = 0;
    while (i < count) {
        int value = seed + (int)i * 3;
        int pushed = ring_buffer_push(value);
        if (pushed == 0) {
            value = ring_buffer_peek();
            total = total - value;
        } else {
            total = total + value;
        }
        i = i + 1;
    }
    return total;
}

int ring_drain(int limit) {
    int drained = 0;
    int guard = 0;
    while (guard < limit) {
        int value = ring_buffer_pop();
        if (value < 0) {
            break;
        }
        drained += clamp_slot(value, limit);
        guard = guard + 1;
    }
    return drained + ring_buffer_capacity();
}

int ring_compact(int mask, int window) {
    int compacted = 0;
    for (int i = 0; i < window; i++) {
        int value = ring_buffer_pop();
        if ((value & mask) != 0) {
            compacted = compacted ^ value;
        } else {
            compacted = compacted | (value << 1);
        }
    }
    return compacted;
}
