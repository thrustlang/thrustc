#define COMMAND_COUNT 9
#define MODE_ADD 1
#define MODE_SUB 2
#define MODE_MIX 3

const int global_seed_09 = 2;
static int branch_hits_09 = 0;

static int dispatch_09(int mode, int value) {
    switch (mode) {
        case MODE_ADD:
            branch_hits_09 += 1;
            return value + global_seed_09;
        case MODE_SUB:
            branch_hits_09 += 1;
            return value - 3;
        case MODE_MIX:
            branch_hits_09 += 1;
            return (value * 3) + 1;
        default:
            return value;
    }
}

static int run_control_09(const int *modes, const int *values) {
    int total = 0;
    for (int i = 0; i < COMMAND_COUNT; ++i) {
        int current = dispatch_09(modes[i], values[i]);
        if ((current % 3) == 0) {
            total += current / 3;
        } else {
            total += current;
        }
        if (total > 36) {
            total -= 2;
        }
    }
    return total;
}

int main(void) {
    int modes[COMMAND_COUNT] = {2, 3, 1, 2, 3, 1, 2, 3, 1};
    int values[COMMAND_COUNT] = {9, 5, 4, 8, 6, 3, 7, 2, 1};
    int total = run_control_09(modes, values);

    if (branch_hits_09 != COMMAND_COUNT) {
        return 1;
    }
    if (total <= 16) {
        return 2;
    }
    if (dispatch_09(MODE_ADD, 1) <= 1) {
        return 3;
    }
    return 0;
}
