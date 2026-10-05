#define ARRAY_LEN 11
#define WINDOW 5
#define THRESHOLD 24
#define STEP_BONUS 1

const int global_offset_09 = 5;
static int processed_windows_09 = 0;

static int normalize_09(int value) {
    if (value < 0) {
        return -value + global_offset_09 + 2;
    }
    return value + global_offset_09;
}

static int sliding_sum_09(const int *values, int start) {
    int total = 0;
    for (int i = 0; i < WINDOW; ++i) {
        total += values[start + i];
    }
    processed_windows_09 += 1;
    return total;
}

static int count_peaks_09(const int *values) {
    int peaks = 0;
    for (int i = 1; i + 1 < ARRAY_LEN; ++i) {
        if (values[i] > values[i - 1] && values[i] >= values[i + 1]) {
            peaks += 1;
        }
    }
    return peaks;
}

int main(void) {
    int raw[ARRAY_LEN] = {4, -2, 7, 3, 9, -1, 5, 8, -3, 6, 2};
    int cooked[ARRAY_LEN];
    int best = 0;
    int score = 0;

    for (int i = 0; i < ARRAY_LEN; ++i) {
        cooked[i] = normalize_09(raw[i]);
        if ((i % 4) == 0) {
            cooked[i] += STEP_BONUS;
        }
    }

    for (int i = 0; i + WINDOW <= ARRAY_LEN; ++i) {
        int current = sliding_sum_09(cooked, i);
        if (current > best) {
            best = current;
        }
        if (current >= THRESHOLD) {
            score += 1;
        }
    }

    score += count_peaks_09(cooked);

    if (processed_windows_09 != ARRAY_LEN - WINDOW + 1) {
        return 1;
    }
    if (best <= THRESHOLD) {
        return 2;
    }
    if (score < 5) {
        return 3;
    }
    return 0;
}
