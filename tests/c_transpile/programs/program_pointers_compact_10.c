#define ITEM_COUNT 10
#define MIN_KEEP 4
#define SHIFT 3

const int pointer_floor_10 = 21;
static int moved_items_10 = 0;

static int compact_positive_10(const int *input, int *output) {
    int count = 0;
    for (int i = 0; i < ITEM_COUNT; ++i) {
        if (input[i] >= 2) {
            output[count] = input[i] + SHIFT;
            count += 1;
            moved_items_10 += 1;
        }
    }
    return count;
}

static int sum_items_10(const int *values, int count) {
    int total = 0;
    for (int i = 0; i < count; ++i) {
        total += values[i];
    }
    return total;
}

int main(void) {
    int input[ITEM_COUNT] = {2, 5, 7, 1, 6, -2, 8, 3, 4, 9};
    int output[ITEM_COUNT] = {0};
    int count = compact_positive_10(input, output);
    int total = sum_items_10(output, count);

    if (count < MIN_KEEP) {
        return 1;
    }
    if (moved_items_10 != count) {
        return 2;
    }
    if (total <= pointer_floor_10) {
        return 3;
    }
    return 0;
}
