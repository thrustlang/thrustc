#define ARRAY_LEN 13
#define WINDOW 4
#define THRESHOLD 19
#define STEP_BONUS 1

const int global_offset_02 = 4;
static int processed_windows_02 = 0;

static int normalize_02(int value) {
    if (value < 0) {
        return -value + global_offset_02;
    }
    return value + global_offset_02;
}

static int sliding_sum_02(const int *values, int start) {
    int total = 0;
    for (int i = 0; i < WINDOW; ++i) {
        total += values[start + i];
    }
    processed_windows_02 += 1;
    return total;
}

static int count_peaks_02(const int *values) {
    int peaks = 0;
    for (int i = 1; i + 1 < ARRAY_LEN; ++i) {
        if (values[i] >= values[i - 1] && values[i] > values[i + 1]) {
            peaks += 1;
        }
    }
    return peaks;
}

int main(void) {
    int raw[ARRAY_LEN] = {1, -4, 6, 2, 8, -3, 5, 7, 0, 9, -1, 4, 3};
    int cooked[ARRAY_LEN];
    int best = 0;
    int score = 0;

    for (int i = 0; i < ARRAY_LEN; ++i) {
        cooked[i] = normalize_02(raw[i]);
        if ((i % 3) == 0) {
            cooked[i] += STEP_BONUS;
        }
    }

    for (int i = 0; i + WINDOW <= ARRAY_LEN; ++i) {
        int current = sliding_sum_02(cooked, i);
        if (current > best) {
            best = current;
        }
        if (current >= THRESHOLD) {
            score += 2;
        }
    }

    score += count_peaks_02(cooked);

    if (processed_windows_02 != ARRAY_LEN - WINDOW + 1) {
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
