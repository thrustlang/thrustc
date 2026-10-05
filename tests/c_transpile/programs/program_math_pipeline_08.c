#define PAIR_COUNT 6
#define SCALE 3
#define LIMIT 46
#define OFFSET 3

const int base_bias_08 = 4;
static int reductions_08 = 0;

static int gcd_08(int a, int b) {
    while (b != 0) {
        int next = a % b;
        a = b;
        b = next;
    }
    return a < 0 ? -a : a;
}

static int fold_pair_08(int left, int right) {
    int common = gcd_08(left + OFFSET, right + OFFSET);
    int score = (left * SCALE) + right + base_bias_08;
    reductions_08 += 1;
    return score - common;
}

static int fold_all_08(const int pairs[PAIR_COUNT][2]) {
    int total = 0;
    for (int i = 0; i < PAIR_COUNT; ++i) {
        int local = fold_pair_08(pairs[i][0], pairs[i][1]);
        if ((local % 3) == 0) {
            total += local / 3;
        } else {
            total += local + 2;
        }
    }
    return total;
}

int main(void) {
    int pairs[PAIR_COUNT][2] = {
        {5, 14}, {7, 8}, {9, 6}, {4, 13}, {10, 2}, {6, 12}
    };
    int total = fold_all_08(pairs);
    int guard = gcd_08(total, LIMIT + 15);

    if (reductions_08 != PAIR_COUNT) {
        return 1;
    }
    if (total <= LIMIT) {
        return 2;
    }
    if (guard <= 0) {
        return 3;
    }
    return 0;
}
