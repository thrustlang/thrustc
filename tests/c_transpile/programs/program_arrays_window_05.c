#define ARRAY_LEN 10
#define WINDOW 4
#define THRESHOLD 18
#define STEP_BONUS 2

const int global_offset_05 = 7;
static int processed_windows_05 = 0;

static int normalize_05(int value) {
    if (value < 0) {
        return -value + global_offset_05;
    }
    return value + global_offset_05 + 1;
}

static int sliding_sum_05(const int *values, int start) {
    int total = 0;
    for (int i = 0; i < WINDOW; ++i) {
        total += values[start + i];
    }
    processed_windows_05 += 1;
    return total;
}

static int count_peaks_05(const int *values) {
    int peaks = 0;
    for (int i = 1; i + 1 < ARRAY_LEN; ++i) {
        if (values[i] > values[i - 1] && values[i] >= values[i + 1]) {
            peaks += 1;
        }
    }
    return peaks;
}

int main(void) {
    int raw[ARRAY_LEN] = {5, -1, 4, 6, -3, 8, 2, 7, -2, 9};
    int cooked[ARRAY_LEN];
    int best = 0;
    int score = 0;

    for (int i = 0; i < ARRAY_LEN; ++i) {
        cooked[i] = normalize_05(raw[i]);
        if ((i % 2) == 1) {
            cooked[i] += STEP_BONUS;
        }
    }

    for (int i = 0; i + WINDOW <= ARRAY_LEN; ++i) {
        int current = sliding_sum_05(cooked, i);
        if (current > best) {
            best = current;
        }
        if (current >= THRESHOLD) {
            score += 1;
        }
    }

    score += count_peaks_05(cooked);

    if (processed_windows_05 != ARRAY_LEN - WINDOW + 1) {
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
