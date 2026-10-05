#define COMMAND_COUNT 8
#define MODE_ADD 1
#define MODE_SUB 2
#define MODE_MIX 3

const int global_seed_07 = 3;
static int branch_hits_07 = 0;

static int dispatch_07(int mode, int value) {
    switch (mode) {
        case MODE_ADD:
            branch_hits_07 += 1;
            return value + global_seed_07;
        case MODE_SUB:
            branch_hits_07 += 1;
            return value - 2;
        case MODE_MIX:
            branch_hits_07 += 1;
            return (value * 2) + global_seed_07;
        default:
            return value;
    }
}

static int run_control_07(const int *modes, const int *values) {
    int total = 0;
    for (int i = 0; i < COMMAND_COUNT; ++i) {
        int current = dispatch_07(modes[i], values[i]);
        if ((current % 5) == 0) {
            total += current / 5;
        } else {
            total += current;
        }
        if (total > 44) {
            total -= 3;
        }
    }
    return total;
}

int main(void) {
    int modes[COMMAND_COUNT] = {3, 1, 2, 3, 1, 2, 3, 1};
    int values[COMMAND_COUNT] = {4, 5, 7, 6, 2, 8, 3, 9};
    int total = run_control_07(modes, values);

    if (branch_hits_07 != COMMAND_COUNT) {
        return 1;
    }
    if (total <= 19) {
        return 2;
    }
    if (dispatch_07(MODE_MIX, 1) <= 1) {
        return 3;
    }
    return 0;
}
