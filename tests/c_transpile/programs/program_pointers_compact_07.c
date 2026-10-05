#define ITEM_COUNT 12
#define MIN_KEEP 5
#define SHIFT 1

const int pointer_floor_07 = 21;
static int moved_items_07 = 0;

static int compact_positive_07(const int *input, int *output) {
    int count = 0;
    for (int i = 0; i < ITEM_COUNT; ++i) {
        if (input[i] > 2) {
            output[count] = input[i] + SHIFT;
            count += 1;
            moved_items_07 += 1;
        }
    }
    return count;
}

static int sum_items_07(const int *values, int count) {
    int total = 0;
    for (int i = 0; i < count; ++i) {
        total += values[i];
    }
    return total;
}

int main(void) {
    int input[ITEM_COUNT] = {3, 6, 2, 7, 4, 1, 8, 5, -2, 9, 10, 0};
    int output[ITEM_COUNT] = {0};
    int count = compact_positive_07(input, output);
    int total = sum_items_07(output, count);

    if (count < MIN_KEEP) {
        return 1;
    }
    if (moved_items_07 != count) {
        return 2;
    }
    if (total <= pointer_floor_07) {
        return 3;
    }
    return 0;
}
