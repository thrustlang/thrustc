#define PAIR_COUNT 6
#define SCALE 3
#define LIMIT 43
#define OFFSET 4

const int base_bias_06 = 5;
static int reductions_06 = 0;

static int gcd_06(int a, int b) {
    while (b != 0) {
        int next = a % b;
        a = b;
        b = next;
    }
    return a < 0 ? -a : a;
}

static int fold_pair_06(int left, int right) {
    int common = gcd_06(left + OFFSET, right + OFFSET);
    int score = (left * SCALE) + right + base_bias_06;
    reductions_06 += 1;
    return score - common;
}

static int fold_all_06(const int pairs[PAIR_COUNT][2]) {
    int total = 0;
    for (int i = 0; i < PAIR_COUNT; ++i) {
        int local = fold_pair_06(pairs[i][0], pairs[i][1]);
        if ((local % 5) == 0) {
            total += local / 5;
        } else {
            total += local + 1;
        }
    }
    return total;
}

int main(void) {
    int pairs[PAIR_COUNT][2] = {
        {7, 6}, {5, 12}, {9, 8}, {4, 11}, {10, 7}, {6, 13}
    };
    int total = fold_all_06(pairs);
    int guard = gcd_06(total, LIMIT + 17);

    if (reductions_06 != PAIR_COUNT) {
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
