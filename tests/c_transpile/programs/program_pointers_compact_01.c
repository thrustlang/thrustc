#define ITEM_COUNT 12
#define MIN_KEEP 3
#define SHIFT 2

const int pointer_floor_01 = 20;
static int moved_items_01 = 0;

static int compact_positive_01(const int *input, int *output) {
    int count = 0;
    for (int i = 0; i < ITEM_COUNT; ++i) {
        if (input[i] > 0) {
            output[count] = input[i] + SHIFT;
            count += 1;
            moved_items_01 += 1;
        }
    }
    return count;
}

static int sum_items_01(const int *values, int count) {
    int total = 0;
    for (int i = 0; i < count; ++i) {
        total += values[i];
    }
    return total;
}

int main(void) {
    int input[ITEM_COUNT] = {3, -1, 5, 0, 7, -2, 4, 6, -3, 8, 1, -4};
    int output[ITEM_COUNT] = {0};
    int count = compact_positive_01(input, output);
    int total = sum_items_01(output, count);

    if (count < MIN_KEEP) {
        return 1;
    }
    if (moved_items_01 != count) {
        return 2;
    }
    if (total <= pointer_floor_01) {
        return 3;
    }
    return 0;
}
