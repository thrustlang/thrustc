#define PAIR_COUNT 7
#define SCALE 4
#define LIMIT 54
#define OFFSET 2

const int base_bias_07 = 6;
static int reductions_07 = 0;

static int gcd_07(int a, int b) {
    while (b != 0) {
        int next = a % b;
        a = b;
        b = next;
    }
    return a < 0 ? -a : a;
}

static int fold_pair_07(int left, int right) {
    int common = gcd_07(left + OFFSET, right + OFFSET);
    int score = (left * SCALE) + right + base_bias_07;
    reductions_07 += 1;
    return score - common;
}

static int fold_all_07(const int pairs[PAIR_COUNT][2]) {
    int total = 0;
    for (int i = 0; i < PAIR_COUNT; ++i) {
        int local = fold_pair_07(pairs[i][0], pairs[i][1]);
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
        {8, 4}, {6, 9}, {10, 5}, {7, 12}, {9, 3}, {5, 11}, {11, 6}
    };
    int total = fold_all_07(pairs);
    int guard = gcd_07(total, LIMIT + 9);

    if (reductions_07 != PAIR_COUNT) {
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
