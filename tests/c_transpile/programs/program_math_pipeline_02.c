#define PAIR_COUNT 7
#define SCALE 2
#define LIMIT 36
#define OFFSET 3

const int base_bias_02 = 5;
static int reductions_02 = 0;

static int gcd_02(int a, int b) {
    while (b != 0) {
        int next = a % b;
        a = b;
        b = next;
    }
    return a < 0 ? -a : a;
}

static int fold_pair_02(int left, int right) {
    int common = gcd_02(left + OFFSET, right + OFFSET);
    int score = (left * SCALE) + right + base_bias_02;
    reductions_02 += 1;
    return score - common;
}

static int fold_all_02(const int pairs[PAIR_COUNT][2]) {
    int total = 0;
    for (int i = 0; i < PAIR_COUNT; ++i) {
        int local = fold_pair_02(pairs[i][0], pairs[i][1]);
        if ((local % 3) == 0) {
            total += local / 3;
        } else {
            total += local;
        }
    }
    return total;
}

int main(void) {
    int pairs[PAIR_COUNT][2] = {
        {3, 8}, {5, 7}, {9, 4}, {6, 10}, {8, 1}, {4, 12}, {7, 5}
    };
    int total = fold_all_02(pairs);
    int guard = gcd_02(total, LIMIT + 11);

    if (reductions_02 != PAIR_COUNT) {
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
