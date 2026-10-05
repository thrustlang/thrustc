#define ARRAY_LEN 12
#define WINDOW 3
#define THRESHOLD 14
#define STEP_BONUS 2

const int global_offset_01 = 5;
static int processed_windows_01 = 0;

static int normalize_01(int value) {
    if (value < 0) {
        return -value + global_offset_01;
    }
    return value + global_offset_01;
}

static int sliding_sum_01(const int *values, int start) {
    int total = 0;
    for (int i = 0; i < WINDOW; ++i) {
        total += values[start + i];
    }
    processed_windows_01 += 1;
    return total;
}

static int count_peaks_01(const int *values) {
    int peaks = 0;
    for (int i = 1; i + 1 < ARRAY_LEN; ++i) {
        if (values[i] > values[i - 1] && values[i] >= values[i + 1]) {
            peaks += 1;
        }
    }
    return peaks;
}

int main(void) {
    int raw[ARRAY_LEN] = {2, -3, 5, 7, 1, 4, -2, 6, 3, 8, -1, 5};
    int cooked[ARRAY_LEN];
    int best = 0;
    int score = 0;

    for (int i = 0; i < ARRAY_LEN; ++i) {
        cooked[i] = normalize_01(raw[i]);
        if ((i % 2) == 0) {
            cooked[i] += STEP_BONUS;
        }
    }

    for (int i = 0; i + WINDOW <= ARRAY_LEN; ++i) {
        int current = sliding_sum_01(cooked, i);
        if (current > best) {
            best = current;
        }
        if (current >= THRESHOLD) {
            score += 1;
        }
    }

    score += count_peaks_01(cooked);

    if (processed_windows_01 != ARRAY_LEN - WINDOW + 1) {
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
