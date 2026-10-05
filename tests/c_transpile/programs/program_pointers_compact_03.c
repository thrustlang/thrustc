#define ITEM_COUNT 10
#define MIN_KEEP 4
#define SHIFT 3

const int pointer_floor_03 = 22;
static int moved_items_03 = 0;

static int compact_positive_03(const int *input, int *output) {
    int count = 0;
    for (int i = 0; i < ITEM_COUNT; ++i) {
        if (input[i] > 1) {
            output[count] = input[i] + SHIFT;
            count += 1;
            moved_items_03 += 1;
        }
    }
    return count;
}

static int sum_items_03(const int *values, int count) {
    int total = 0;
    for (int i = 0; i < count; ++i) {
        total += values[i];
    }
    return total;
}

int main(void) {
    int input[ITEM_COUNT] = {4, 2, -1, 7, 3, 0, 8, 5, -3, 6};
    int output[ITEM_COUNT] = {0};
    int count = compact_positive_03(input, output);
    int total = sum_items_03(output, count);

    if (count < MIN_KEEP) {
        return 1;
    }
    if (moved_items_03 != count) {
        return 2;
    }
    if (total <= pointer_floor_03) {
        return 3;
    }
    return 0;
}
