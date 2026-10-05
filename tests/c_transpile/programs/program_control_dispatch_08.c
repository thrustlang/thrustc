#define COMMAND_COUNT 6
#define MODE_ADD 1
#define MODE_SUB 2
#define MODE_MIX 3

const int global_seed_08 = 5;
static int branch_hits_08 = 0;

static int dispatch_08(int mode, int value) {
    switch (mode) {
        case MODE_ADD:
            branch_hits_08 += 1;
            return value + global_seed_08;
        case MODE_SUB:
            branch_hits_08 += 1;
            return value - 1;
        case MODE_MIX:
            branch_hits_08 += 1;
            return (value * 4) - 3;
        default:
            return value;
    }
}

static int run_control_08(const int *modes, const int *values) {
    int total = 0;
    for (int i = 0; i < COMMAND_COUNT; ++i) {
        int current = dispatch_08(modes[i], values[i]);
        if ((current & 1) == 0) {
            total += current / 2;
        } else {
            total += current;
        }
        if (total > 38) {
            total -= 4;
        }
    }
    return total;
}

int main(void) {
    int modes[COMMAND_COUNT] = {1, 3, 2, 1, 3, 2};
    int values[COMMAND_COUNT] = {3, 7, 8, 5, 6, 4};
    int total = run_control_08(modes, values);

    if (branch_hits_08 != COMMAND_COUNT) {
        return 1;
    }
    if (total <= 18) {
        return 2;
    }
    if (dispatch_08(MODE_SUB, 3) >= 3) {
        return 3;
    }
    return 0;
}
