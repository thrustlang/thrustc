#define PAIR_COUNT 6
#define SCALE 3
#define LIMIT 40
#define OFFSET 2

const int base_bias_01 = 7;
static int reductions_01 = 0;

static int gcd_01(int a, int b) {
    while (b != 0) {
        int next = a % b;
        a = b;
        b = next;
    }
    return a < 0 ? -a : a;
}

static int fold_pair_01(int left, int right) {
    int common = gcd_01(left + OFFSET, right + OFFSET);
    int score = (left * SCALE) + right + base_bias_01;
    reductions_01 += 1;
    return score - common;
}

static int fold_all_01(const int pairs[PAIR_COUNT][2]) {
    int total = 0;
    for (int i = 0; i < PAIR_COUNT; ++i) {
        int local = fold_pair_01(pairs[i][0], pairs[i][1]);
        if ((local & 1) == 0) {
            total += local / 2;
        } else {
            total += local;
        }
    }
    return total;
}

int main(void) {
    int pairs[PAIR_COUNT][2] = {
        {4, 9}, {6, 5}, {8, 3}, {7, 11}, {5, 10}, {9, 2}
    };
    int total = fold_all_01(pairs);
    int guard = gcd_01(total, LIMIT + 9);

    if (reductions_01 != PAIR_COUNT) {
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
