#define PAIR_COUNT 6
#define SCALE 4
#define LIMIT 52
#define OFFSET 1

const int base_bias_03 = 6;
static int reductions_03 = 0;

static int gcd_03(int a, int b) {
    while (b != 0) {
        int next = a % b;
        a = b;
        b = next;
    }
    return a < 0 ? -a : a;
}

static int fold_pair_03(int left, int right) {
    int common = gcd_03(left + OFFSET, right + OFFSET);
    int score = (left * SCALE) + right + base_bias_03;
    reductions_03 += 1;
    return score - common;
}

static int fold_all_03(const int pairs[PAIR_COUNT][2]) {
    int total = 0;
    for (int i = 0; i < PAIR_COUNT; ++i) {
        int local = fold_pair_03(pairs[i][0], pairs[i][1]);
        if ((local & 1) != 0) {
            total += local + OFFSET;
        } else {
            total += local / 2;
        }
    }
    return total;
}

int main(void) {
    int pairs[PAIR_COUNT][2] = {
        {5, 11}, {7, 9}, {4, 13}, {8, 6}, {10, 3}, {6, 12}
    };
    int total = fold_all_03(pairs);
    int guard = gcd_03(total, LIMIT + 5);

    if (reductions_03 != PAIR_COUNT) {
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
