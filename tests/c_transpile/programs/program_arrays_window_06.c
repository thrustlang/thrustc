#define ARRAY_LEN 15
#define WINDOW 5
#define THRESHOLD 29
#define STEP_BONUS 1

const int global_offset_06 = 4;
static int processed_windows_06 = 0;

static int normalize_06(int value) {
    if (value < 0) {
        return (-value * 2) + global_offset_06;
    }
    return value + global_offset_06;
}

static int sliding_sum_06(const int *values, int start) {
    int total = 0;
    for (int i = 0; i < WINDOW; ++i) {
        total += values[start + i];
    }
    processed_windows_06 += 1;
    return total;
}

static int count_peaks_06(const int *values) {
    int peaks = 0;
    for (int i = 1; i + 1 < ARRAY_LEN; ++i) {
        if (values[i] >= values[i - 1] && values[i] > values[i + 1]) {
            peaks += 1;
        }
    }
    return peaks;
}

int main(void) {
    int raw[ARRAY_LEN] = {1, -6, 3, 8, 4, -2, 7, 5, 9, -1, 2, 6, 8, -3, 4};
    int cooked[ARRAY_LEN];
    int best = 0;
    int score = 0;

    for (int i = 0; i < ARRAY_LEN; ++i) {
        cooked[i] = normalize_06(raw[i]);
        if ((i % 5) == 2) {
            cooked[i] += STEP_BONUS;
        }
    }

    for (int i = 0; i + WINDOW <= ARRAY_LEN; ++i) {
        int current = sliding_sum_06(cooked, i);
        if (current > best) {
            best = current;
        }
        if (current >= THRESHOLD) {
            score += 2;
        }
    }

    score += count_peaks_06(cooked);

    if (processed_windows_06 != ARRAY_LEN - WINDOW + 1) {
        return 1;
    }
    if (best <= THRESHOLD) {
        return 2;
    }
    if (score < 9) {
        return 3;
    }
    return 0;
}
