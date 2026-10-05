#define ITEM_COUNT 11
#define MIN_KEEP 4
#define SHIFT 1

const int pointer_floor_02 = 18;
static int moved_items_02 = 0;

static int compact_positive_02(const int *input, int *output) {
    int count = 0;
    for (int i = 0; i < ITEM_COUNT; ++i) {
        if (input[i] >= 2) {
            output[count] = input[i] + SHIFT;
            count += 1;
            moved_items_02 += 1;
        }
    }
    return count;
}

static int sum_items_02(const int *values, int count) {
    int total = 0;
    for (int i = 0; i < count; ++i) {
        total += values[i];
    }
    return total;
}

int main(void) {
    int input[ITEM_COUNT] = {2, -2, 6, 1, 5, 3, -1, 7, 4, 0, 8};
    int output[ITEM_COUNT] = {0};
    int count = compact_positive_02(input, output);
    int total = sum_items_02(output, count);

    if (count < MIN_KEEP) {
        return 1;
    }
    if (moved_items_02 != count) {
        return 2;
    }
    if (total <= pointer_floor_02) {
        return 3;
    }
    return 0;
}
