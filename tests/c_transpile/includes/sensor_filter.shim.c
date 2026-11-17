#include "sensor_filter.h"
#include "filter_math.h"

int sensor_read(int channel) { return channel * 2 + 10; }
int sensor_scale(int raw, int factor) { return raw + factor; }

int filter_clamp(int value, int low, int high) {
    if (value < low) { return low; }
    if (value > high) { return high; }
    return value;
}

int filter_average(int total, int count) {
    if (count == 0) { return 0; }
    return total / count;
}
