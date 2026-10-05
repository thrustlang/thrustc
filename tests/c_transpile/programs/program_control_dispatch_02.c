#define COMMAND_COUNT 7
#define MODE_ADD 1
#define MODE_SUB 2
#define MODE_MIX 3

const int global_seed_02 = 3;
static int branch_hits_02 = 0;

static int dispatch_02(int mode, int value) {
    switch (mode) {
        case MODE_ADD:
            branch_hits_02 += 1;
            return value + global_seed_02;
        case MODE_SUB:
            branch_hits_02 += 1;
            return value - 2;
        case MODE_MIX:
            branch_hits_02 += 1;
            return (value * 3) - global_seed_02;
        default:
            return value;
    }
}

static int run_control_02(const int *modes, const int *values) {
    int total = 0;
    for (int i = 0; i < COMMAND_COUNT; ++i) {
        int current = dispatch_02(modes[i], values[i]);
        if ((current % 3) == 0) {
            total += current / 3;
        } else {
            total += current;
        }
        if (total > 35) {
            total -= 2;
        }
    }
    return total;
}

int main(void) {
    int modes[COMMAND_COUNT] = {3, 1, 2, 3, 1, 2, 3};
    int values[COMMAND_COUNT] = {4, 7, 6, 5, 8, 3, 9};
    int total = run_control_02(modes, values);

    if (branch_hits_02 != COMMAND_COUNT) {
        return 1;
    }
    if (total <= 22) {
        return 2;
    }
    if (dispatch_02(MODE_SUB, 5) >= 5) {
        return 3;
    }
    return 0;
}
