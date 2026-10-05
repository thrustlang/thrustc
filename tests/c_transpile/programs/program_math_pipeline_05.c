#define PAIR_COUNT 5
#define SCALE 5
#define LIMIT 47
#define OFFSET 1

const int base_bias_05 = 8;
static int reductions_05 = 0;

static int gcd_05(int a, int b) {
    while (b != 0) {
        int next = a % b;
        a = b;
        b = next;
    }
    return a < 0 ? -a : a;
}

static int fold_pair_05(int left, int right) {
    int common = gcd_05(left + OFFSET, right + OFFSET);
    int score = (left * SCALE) + right + base_bias_05;
    reductions_05 += 1;
    return score - common;
}

static int fold_all_05(const int pairs[PAIR_COUNT][2]) {
    int total = 0;
    for (int i = 0; i < PAIR_COUNT; ++i) {
        int local = fold_pair_05(pairs[i][0], pairs[i][1]);
        if ((local & 1) == 0) {
            total += local / 2;
        } else {
            total += local + 2;
        }
    }
    return total;
}

int main(void) {
    int pairs[PAIR_COUNT][2] = {
        {6, 4}, {8, 7}, {5, 9}, {11, 3}, {7, 10}
    };
    int total = fold_all_05(pairs);
    int guard = gcd_05(total, LIMIT + 7);

    if (reductions_05 != PAIR_COUNT) {
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
