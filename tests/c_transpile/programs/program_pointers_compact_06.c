#define ITEM_COUNT 10
#define MIN_KEEP 4
#define SHIFT 2

const int pointer_floor_06 = 19;
static int moved_items_06 = 0;

static int compact_positive_06(const int *input, int *output) {
    int count = 0;
    for (int i = 0; i < ITEM_COUNT; ++i) {
        if (input[i] >= 2) {
            output[count] = input[i] + SHIFT;
            count += 1;
            moved_items_06 += 1;
        }
    }
    return count;
}

static int sum_items_06(const int *values, int count) {
    int total = 0;
    for (int i = 0; i < count; ++i) {
        total += values[i];
    }
    return total;
}

int main(void) {
    int input[ITEM_COUNT] = {2, 5, 1, 6, -3, 7, 4, 0, 8, 3};
    int output[ITEM_COUNT] = {0};
    int count = compact_positive_06(input, output);
    int total = sum_items_06(output, count);

    if (count < MIN_KEEP) {
        return 1;
    }
    if (moved_items_06 != count) {
        return 2;
    }
    if (total <= pointer_floor_06) {
        return 3;
    }
    return 0;
}
