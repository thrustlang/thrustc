#define ARRAY_LEN 12
#define WINDOW 4
#define THRESHOLD 20
#define STEP_BONUS 3

const int global_offset_07 = 2;
static int processed_windows_07 = 0;

static int normalize_07(int value) {
    if (value < 0) {
        return -value + global_offset_07 + STEP_BONUS;
    }
    return value + global_offset_07;
}

static int sliding_sum_07(const int *values, int start) {
    int total = 0;
    for (int i = 0; i < WINDOW; ++i) {
        total += values[start + i];
    }
    processed_windows_07 += 1;
    return total;
}

static int count_peaks_07(const int *values) {
    int peaks = 0;
    for (int i = 1; i + 1 < ARRAY_LEN; ++i) {
        if (values[i] > values[i - 1] && values[i] > values[i + 1]) {
            peaks += 1;
        }
    }
    return peaks;
}

int main(void) {
    int raw[ARRAY_LEN] = {7, -2, 5, 1, 8, -4, 6, 3, 9, -1, 4, 2};
    int cooked[ARRAY_LEN];
    int best = 0;
    int score = 0;

    for (int i = 0; i < ARRAY_LEN; ++i) {
        cooked[i] = normalize_07(raw[i]);
        if ((i & 1) == 0) {
            cooked[i] += STEP_BONUS;
        }
    }

    for (int i = 0; i + WINDOW <= ARRAY_LEN; ++i) {
        int current = sliding_sum_07(cooked, i);
        if (current > best) {
            best = current;
        }
        if (current >= THRESHOLD) {
            score += 1;
        }
    }

    score += count_peaks_07(cooked);

    if (processed_windows_07 != ARRAY_LEN - WINDOW + 1) {
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
