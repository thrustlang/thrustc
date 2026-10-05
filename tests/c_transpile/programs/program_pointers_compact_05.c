#define ITEM_COUNT 11
#define MIN_KEEP 3
#define SHIFT 4

const int pointer_floor_05 = 24;
static int moved_items_05 = 0;

static int compact_positive_05(const int *input, int *output) {
    int count = 0;
    for (int i = 0; i < ITEM_COUNT; ++i) {
        if (input[i] > 0) {
            output[count] = input[i] + SHIFT;
            count += 1;
            moved_items_05 += 1;
        }
    }
    return count;
}

static int sum_items_05(const int *values, int count) {
    int total = 0;
    for (int i = 0; i < count; ++i) {
        total += values[i];
    }
    return total;
}

int main(void) {
    int input[ITEM_COUNT] = {1, 4, -2, 6, 3, -1, 7, 5, 2, 8, 9};
    int output[ITEM_COUNT] = {0};
    int count = compact_positive_05(input, output);
    int total = sum_items_05(output, count);

    if (count < MIN_KEEP) {
        return 1;
    }
    if (moved_items_05 != count) {
        return 2;
    }
    if (total <= pointer_floor_05) {
        return 3;
    }
    return 0;
}
