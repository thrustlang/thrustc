#define ARRAY_LEN 14
#define WINDOW 5
#define THRESHOLD 26
#define STEP_BONUS 2

const int global_offset_04 = 3;
static int processed_windows_04 = 0;

static int normalize_04(int value) {
    if (value < 0) {
        return -value + global_offset_04 + 1;
    }
    return value + global_offset_04;
}

static int sliding_sum_04(const int *values, int start) {
    int total = 0;
    for (int i = 0; i < WINDOW; ++i) {
        total += values[start + i];
    }
    processed_windows_04 += 1;
    return total;
}

static int count_peaks_04(const int *values) {
    int peaks = 0;
    for (int i = 1; i + 1 < ARRAY_LEN; ++i) {
        if (values[i] >= values[i - 1] && values[i] >= values[i + 1]) {
            peaks += 1;
        }
    }
    return peaks;
}

int main(void) {
    int raw[ARRAY_LEN] = {4, -5, 2, 7, 3, 8, -2, 6, 1, 9, 5, -1, 4, 7};
    int cooked[ARRAY_LEN];
    int best = 0;
    int score = 0;

    for (int i = 0; i < ARRAY_LEN; ++i) {
        cooked[i] = normalize_04(raw[i]);
        if ((i % 4) == 1) {
            cooked[i] += STEP_BONUS;
        }
    }

    for (int i = 0; i + WINDOW <= ARRAY_LEN; ++i) {
        int current = sliding_sum_04(cooked, i);
        if (current > best) {
            best = current;
        }
        if (current >= THRESHOLD) {
            score += 2;
        }
    }

    score += count_peaks_04(cooked);

    if (processed_windows_04 != ARRAY_LEN - WINDOW + 1) {
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
