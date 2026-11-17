#include "ring_buffer.h"

static int buffer[4];
static int head = 0;
static int tail = 0;
static int count = 0;

int ring_buffer_push(int value) {
    if (count == 4) { return 0; }
    buffer[tail] = value;
    tail = (tail + 1) % 4;
    count++;
    return 1;
}

int ring_buffer_pop(void) {
    int value;
    if (count == 0) { return -1; }
    value = buffer[head];
    head = (head + 1) % 4;
    count--;
    return value;
}

int ring_buffer_peek(void) {
    if (count == 0) { return -1; }
    return buffer[head];
}

int ring_buffer_capacity(void) { return 4; }
