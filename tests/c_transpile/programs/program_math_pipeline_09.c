#define PAIR_COUNT 8
#define SCALE 2
#define LIMIT 41
#define OFFSET 2

const int base_bias_09 = 9;
static int reductions_09 = 0;

static int gcd_09(int a, int b) {
    while (b != 0) {
        int next = a % b;
        a = b;
        b = next;
    }
    return a < 0 ? -a : a;
}

static int fold_pair_09(int left, int right) {
    int common = gcd_09(left + OFFSET, right + OFFSET);
    int score = (left * SCALE) + right + base_bias_09;
    reductions_09 += 1;
    return score - common;
}

static int fold_all_09(const int pairs[PAIR_COUNT][2]) {
    int total = 0;
    for (int i = 0; i < PAIR_COUNT; ++i) {
        int local = fold_pair_09(pairs[i][0], pairs[i][1]);
        if ((local & 1) == 0) {
            total += local / 2;
        } else {
            total += local + 1;
        }
    }
    return total;
}

int main(void) {
    int pairs[PAIR_COUNT][2] = {
        {3, 10}, {6, 8}, {7, 9}, {4, 11}, {8, 5}, {10, 4}, {5, 13}, {9, 6}
    };
    int total = fold_all_09(pairs);
    int guard = gcd_09(total, LIMIT + 19);

    if (reductions_09 != PAIR_COUNT) {
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
