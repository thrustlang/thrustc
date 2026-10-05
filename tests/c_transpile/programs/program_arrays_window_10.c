#define ARRAY_LEN 13
#define WINDOW 4
#define THRESHOLD 23
#define STEP_BONUS 2

const int global_offset_10 = 6;
static int processed_windows_10 = 0;

static int normalize_10(int value) {
    if (value < 0) {
        return (-value * 2) + global_offset_10;
    }
    return value + global_offset_10;
}

static int sliding_sum_10(const int *values, int start) {
    int total = 0;
    for (int i = 0; i < WINDOW; ++i) {
        total += values[start + i];
    }
    processed_windows_10 += 1;
    return total;
}

static int count_peaks_10(const int *values) {
    int peaks = 0;
    for (int i = 1; i + 1 < ARRAY_LEN; ++i) {
        if (values[i] >= values[i - 1] && values[i] > values[i + 1]) {
            peaks += 1;
        }
    }
    return peaks;
}

int main(void) {
    int raw[ARRAY_LEN] = {6, -2, 1, 8, 4, -5, 7, 3, 9, -1, 5, 2, 6};
    int cooked[ARRAY_LEN];
    int best = 0;
    int score = 0;

    for (int i = 0; i < ARRAY_LEN; ++i) {
        cooked[i] = normalize_10(raw[i]);
        if ((i % 2) == 0) {
            cooked[i] += STEP_BONUS;
        }
    }

    for (int i = 0; i + WINDOW <= ARRAY_LEN; ++i) {
        int current = sliding_sum_10(cooked, i);
        if (current > best) {
            best = current;
        }
        if (current >= THRESHOLD) {
            score += 2;
        }
    }

    score += count_peaks_10(cooked);

    if (processed_windows_10 != ARRAY_LEN - WINDOW + 1) {
        return 1;
    }
    if (best <= THRESHOLD) {
        return 2;
    }
    if (score < 8) {
        return 3;
    }
    return 0;
}
