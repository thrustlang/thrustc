#include "sensor_filter.h"
#include "filter_math.h"

int sensor_sweep(int channels, int factor) {
    int accum = 0;
    int channel = 0;
    while (channel < channels) {
        int raw = sensor_read(channel);
        int scaled = sensor_scale(raw, factor);
        if (scaled > 1000) {
            scaled = filter_clamp(scaled, 0, 1000);
        } else if (scaled < 0) {
            scaled = 0;
        }
        accum += scaled;
        channel += 1;
    }
    return filter_average(accum, channels);
}

int sensor_merge(int left, int right, int factor) {
    int merged = 0;
    for (int i = 0; i < factor; i++) {
        int a = sensor_read(left + i);
        int b = sensor_read(right + i);
        int clamped = filter_clamp(a - b, -500, 500);
        merged = merged + (clamped * (i + 1));
    }
    return filter_average(merged, factor);
}

int sensor_calibrate(int sample, int factor) {
    int corrected = sample;
    int steps = 0;
    while (steps < factor && corrected > 0) {
        corrected = corrected - sensor_scale(corrected, factor);
        if (corrected < 0) {
            corrected = filter_clamp(corrected, 0, sample);
        }
        steps += 1;
    }
    return filter_average(corrected, steps + 1);
}
