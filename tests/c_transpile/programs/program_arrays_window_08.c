#define ARRAY_LEN 16
#define WINDOW 6
#define THRESHOLD 34
#define STEP_BONUS 2

const int global_offset_08 = 3;
static int processed_windows_08 = 0;

static int normalize_08(int value) {
    if (value < 0) {
        return (-value * 2) + global_offset_08;
    }
    return value + global_offset_08 + 1;
}

static int sliding_sum_08(const int *values, int start) {
    int total = 0;
    for (int i = 0; i < WINDOW; ++i) {
        total += values[start + i];
    }
    processed_windows_08 += 1;
    return total;
}

static int count_peaks_08(const int *values) {
    int peaks = 0;
    for (int i = 1; i + 1 < ARRAY_LEN; ++i) {
        if (values[i] >= values[i - 1] && values[i] >= values[i + 1]) {
            peaks += 1;
        }
    }
    return peaks;
}

int main(void) {
    int raw[ARRAY_LEN] = {2, -3, 6, 4, 8, -1, 7, 5, 9, -2, 3, 6, 8, -4, 5, 7};
    int cooked[ARRAY_LEN];
    int best = 0;
    int score = 0;

    for (int i = 0; i < ARRAY_LEN; ++i) {
        cooked[i] = normalize_08(raw[i]);
        if ((i % 3) == 1) {
            cooked[i] += STEP_BONUS;
        }
    }

    for (int i = 0; i + WINDOW <= ARRAY_LEN; ++i) {
        int current = sliding_sum_08(cooked, i);
        if (current > best) {
            best = current;
        }
        if (current >= THRESHOLD) {
            score += 2;
        }
    }

    score += count_peaks_08(cooked);

    if (processed_windows_08 != ARRAY_LEN - WINDOW + 1) {
        return 1;
    }
    if (best <= THRESHOLD) {
        return 2;
    }
    if (score < 10) {
        return 3;
    }
    return 0;
}
