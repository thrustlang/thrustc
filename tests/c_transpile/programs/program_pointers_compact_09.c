#define ITEM_COUNT 13
#define MIN_KEEP 5
#define SHIFT 2

const int pointer_floor_09 = 27;
static int moved_items_09 = 0;

static int compact_positive_09(const int *input, int *output) {
    int count = 0;
    for (int i = 0; i < ITEM_COUNT; ++i) {
        if (input[i] > 0) {
            output[count] = input[i] + SHIFT;
            count += 1;
            moved_items_09 += 1;
        }
    }
    return count;
}

static int sum_items_09(const int *values, int count) {
    int total = 0;
    for (int i = 0; i < count; ++i) {
        total += values[i];
    }
    return total;
}

int main(void) {
    int input[ITEM_COUNT] = {4, -2, 6, 1, 8, 3, 7, -1, 5, 9, 2, 10, 11};
    int output[ITEM_COUNT] = {0};
    int count = compact_positive_09(input, output);
    int total = sum_items_09(output, count);

    if (count < MIN_KEEP) {
        return 1;
    }
    if (moved_items_09 != count) {
        return 2;
    }
    if (total <= pointer_floor_09) {
        return 3;
    }
    return 0;
}
