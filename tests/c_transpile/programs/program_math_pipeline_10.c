#define PAIR_COUNT 5
#define SCALE 6
#define LIMIT 55
#define OFFSET 1

const int base_bias_10 = 3;
static int reductions_10 = 0;

static int gcd_10(int a, int b) {
    while (b != 0) {
        int next = a % b;
        a = b;
        b = next;
    }
    return a < 0 ? -a : a;
}

static int fold_pair_10(int left, int right) {
    int common = gcd_10(left + OFFSET, right + OFFSET);
    int score = (left * SCALE) + right + base_bias_10;
    reductions_10 += 1;
    return score - common;
}

static int fold_all_10(const int pairs[PAIR_COUNT][2]) {
    int total = 0;
    for (int i = 0; i < PAIR_COUNT; ++i) {
        int local = fold_pair_10(pairs[i][0], pairs[i][1]);
        if ((local % 4) == 0) {
            total += local / 2;
        } else {
            total += local + 2;
        }
    }
    return total;
}

int main(void) {
    int pairs[PAIR_COUNT][2] = {
        {6, 7}, {8, 5}, {9, 4}, {11, 2}, {10, 6}
    };
    int total = fold_all_10(pairs);
    int guard = gcd_10(total, LIMIT + 21);

    if (reductions_10 != PAIR_COUNT) {
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
