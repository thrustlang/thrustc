#define COMMAND_COUNT 7
#define MODE_ADD 1
#define MODE_SUB 2
#define MODE_MIX 3

const int global_seed_06 = 4;
static int branch_hits_06 = 0;

static int dispatch_06(int mode, int value) {
    switch (mode) {
        case MODE_ADD:
            branch_hits_06 += 1;
            return value + global_seed_06;
        case MODE_SUB:
            branch_hits_06 += 1;
            return value - 1;
        case MODE_MIX:
            branch_hits_06 += 1;
            return (value * 3) - 2;
        default:
            return value;
    }
}

static int run_control_06(const int *modes, const int *values) {
    int total = 0;
    for (int i = 0; i < COMMAND_COUNT; ++i) {
        int current = dispatch_06(modes[i], values[i]);
        if ((current % 3) == 0) {
            total += current / 3;
        } else {
            total += current;
        }
        if (total > 32) {
            total -= 2;
        }
    }
    return total;
}

int main(void) {
    int modes[COMMAND_COUNT] = {2, 1, 3, 2, 1, 3, 1};
    int values[COMMAND_COUNT] = {8, 2, 6, 7, 3, 9, 4};
    int total = run_control_06(modes, values);

    if (branch_hits_06 != COMMAND_COUNT) {
        return 1;
    }
    if (total <= 17) {
        return 2;
    }
    if (dispatch_06(MODE_ADD, 0) != global_seed_06) {
        return 3;
    }
    return 0;
}
