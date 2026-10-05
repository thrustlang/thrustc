#define COMMAND_COUNT 8
#define MODE_ADD 1
#define MODE_SUB 2
#define MODE_MIX 3

const int global_seed_03 = 5;
static int branch_hits_03 = 0;

static int dispatch_03(int mode, int value) {
    switch (mode) {
        case MODE_ADD:
            branch_hits_03 += 1;
            return value + global_seed_03;
        case MODE_SUB:
            branch_hits_03 += 1;
            return value - 1;
        case MODE_MIX:
            branch_hits_03 += 1;
            return (value * 2) + 1;
        default:
            return value;
    }
}

static int run_control_03(const int *modes, const int *values) {
    int total = 0;
    for (int i = 0; i < COMMAND_COUNT; ++i) {
        int current = dispatch_03(modes[i], values[i]);
        if ((current & 1) == 0) {
            total += current / 2;
        } else {
            total += current;
        }
        if (total > 45) {
            total -= 4;
        }
    }
    return total;
}

int main(void) {
    int modes[COMMAND_COUNT] = {1, 2, 3, 1, 2, 3, 1, 3};
    int values[COMMAND_COUNT] = {6, 4, 7, 5, 3, 8, 2, 9};
    int total = run_control_03(modes, values);

    if (branch_hits_03 != COMMAND_COUNT) {
        return 1;
    }
    if (total <= 25) {
        return 2;
    }
    if (dispatch_03(MODE_MIX, 2) <= 2) {
        return 3;
    }
    return 0;
}
