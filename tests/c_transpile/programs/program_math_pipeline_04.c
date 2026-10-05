#define PAIR_COUNT 8
#define SCALE 2
#define LIMIT 44
#define OFFSET 2

const int base_bias_04 = 4;
static int reductions_04 = 0;

static int gcd_04(int a, int b) {
    while (b != 0) {
        int next = a % b;
        a = b;
        b = next;
    }
    return a < 0 ? -a : a;
}

static int fold_pair_04(int left, int right) {
    int common = gcd_04(left + OFFSET, right + OFFSET);
    int score = (left * SCALE) + right + base_bias_04;
    reductions_04 += 1;
    return score - common;
}

static int fold_all_04(const int pairs[PAIR_COUNT][2]) {
    int total = 0;
    for (int i = 0; i < PAIR_COUNT; ++i) {
        int local = fold_pair_04(pairs[i][0], pairs[i][1]);
        if ((local % 4) == 0) {
            total += local / 2;
        } else {
            total += local + 1;
        }
    }
    return total;
}

int main(void) {
    int pairs[PAIR_COUNT][2] = {
        {2, 9}, {5, 8}, {7, 4}, {11, 6}, {4, 10}, {8, 3}, {6, 12}, {9, 5}
    };
    int total = fold_all_04(pairs);
    int guard = gcd_04(total, LIMIT + 13);

    if (reductions_04 != PAIR_COUNT) {
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
