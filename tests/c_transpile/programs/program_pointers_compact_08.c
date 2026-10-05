#define ITEM_COUNT 9
#define MIN_KEEP 3
#define SHIFT 5

const int pointer_floor_08 = 23;
static int moved_items_08 = 0;

static int compact_positive_08(const int *input, int *output) {
    int count = 0;
    for (int i = 0; i < ITEM_COUNT; ++i) {
        if (input[i] >= 1) {
            output[count] = input[i] + SHIFT;
            count += 1;
            moved_items_08 += 1;
        }
    }
    return count;
}

static int sum_items_08(const int *values, int count) {
    int total = 0;
    for (int i = 0; i < count; ++i) {
        total += values[i];
    }
    return total;
}

int main(void) {
    int input[ITEM_COUNT] = {1, 3, -1, 5, 7, 2, 6, 4, 8};
    int output[ITEM_COUNT] = {0};
    int count = compact_positive_08(input, output);
    int total = sum_items_08(output, count);

    if (count < MIN_KEEP) {
        return 1;
    }
    if (moved_items_08 != count) {
        return 2;
    }
    if (total <= pointer_floor_08) {
        return 3;
    }
    return 0;
}
