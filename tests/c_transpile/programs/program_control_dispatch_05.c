#define COMMAND_COUNT 9
#define MODE_ADD 1
#define MODE_SUB 2
#define MODE_MIX 3

const int global_seed_05 = 6;
static int branch_hits_05 = 0;

static int dispatch_05(int mode, int value) {
    switch (mode) {
        case MODE_ADD:
            branch_hits_05 += 1;
            return value + global_seed_05;
        case MODE_SUB:
            branch_hits_05 += 1;
            return value - 2;
        case MODE_MIX:
            branch_hits_05 += 1;
            return (value * 2) - 1;
        default:
            return value;
    }
}

static int run_control_05(const int *modes, const int *values) {
    int total = 0;
    for (int i = 0; i < COMMAND_COUNT; ++i) {
        int current = dispatch_05(modes[i], values[i]);
        if ((current & 1) != 0) {
            total += current;
        } else {
            total += current / 2;
        }
        if (total > 50) {
            total -= 5;
        }
    }
    return total;
}

int main(void) {
    int modes[COMMAND_COUNT] = {1, 3, 1, 2, 3, 2, 1, 3, 2};
    int values[COMMAND_COUNT] = {4, 7, 2, 9, 5, 6, 3, 8, 1};
    int total = run_control_05(modes, values);

    if (branch_hits_05 != COMMAND_COUNT) {
        return 1;
    }
    if (total <= 25) {
        return 2;
    }
    if (dispatch_05(MODE_SUB, 4) >= 4) {
        return 3;
    }
    return 0;
}
