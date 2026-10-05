#define ARRAY_LEN 11
#define WINDOW 3
#define THRESHOLD 16
#define STEP_BONUS 3

const int global_offset_03 = 6;
static int processed_windows_03 = 0;

static int normalize_03(int value) {
    if (value < 0) {
        return (-value * 2) + global_offset_03;
    }
    return value + global_offset_03;
}

static int sliding_sum_03(const int *values, int start) {
    int total = 0;
    for (int i = 0; i < WINDOW; ++i) {
        total += values[start + i];
    }
    processed_windows_03 += 1;
    return total;
}

static int count_peaks_03(const int *values) {
    int peaks = 0;
    for (int i = 1; i + 1 < ARRAY_LEN; ++i) {
        if (values[i] > values[i - 1] && values[i] > values[i + 1]) {
            peaks += 1;
        }
    }
    return peaks;
}

int main(void) {
    int raw[ARRAY_LEN] = {3, -2, 9, 1, 5, -4, 7, 2, 6, -1, 8};
    int cooked[ARRAY_LEN];
    int best = 0;
    int score = 0;

    for (int i = 0; i < ARRAY_LEN; ++i) {
        cooked[i] = normalize_03(raw[i]);
        if ((i & 1) != 0) {
            cooked[i] += STEP_BONUS;
        }
    }

    for (int i = 0; i + WINDOW <= ARRAY_LEN; ++i) {
        int current = sliding_sum_03(cooked, i);
        if (current > best) {
            best = current;
        }
        if (current >= THRESHOLD) {
            score += 1;
        }
    }

    score += count_peaks_03(cooked);

    if (processed_windows_03 != ARRAY_LEN - WINDOW + 1) {
        return 1;
    }
    if (best <= THRESHOLD) {
        return 2;
    }
    if (score < 6) {
        return 3;
    }
    return 0;
}
