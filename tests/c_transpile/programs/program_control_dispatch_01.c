#define COMMAND_COUNT 8
#define MODE_ADD 1
#define MODE_SUB 2
#define MODE_MIX 3

const int global_seed_01 = 4;
static int branch_hits_01 = 0;

static int dispatch_01(int mode, int value) {
    switch (mode) {
        case MODE_ADD:
            branch_hits_01 += 1;
            return value + global_seed_01;
        case MODE_SUB:
            branch_hits_01 += 1;
            return value - 1;
        case MODE_MIX:
            branch_hits_01 += 1;
            return (value * 2) - global_seed_01;
        default:
            return value;
    }
}

static int run_control_01(const int *modes, const int *values) {
    int total = 0;
    for (int i = 0; i < COMMAND_COUNT; ++i) {
        int current = dispatch_01(modes[i], values[i]);
        if ((current % 2) == 0) {
            total += current / 2;
        } else {
            total += current;
        }
        if (total > 40) {
            total -= 3;
        }
    }
    return total;
}

int main(void) {
    int modes[COMMAND_COUNT] = {1, 3, 2, 1, 3, 2, 1, 3};
    int values[COMMAND_COUNT] = {5, 6, 9, 3, 8, 4, 7, 2};
    int total = run_control_01(modes, values);

    if (branch_hits_01 != COMMAND_COUNT) {
        return 1;
    }
    if (total <= 20) {
        return 2;
    }
    if (dispatch_01(MODE_ADD, 1) <= 1) {
        return 3;
    }
    return 0;
}
