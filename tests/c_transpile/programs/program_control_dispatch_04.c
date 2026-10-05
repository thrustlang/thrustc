#define COMMAND_COUNT 6
#define MODE_ADD 1
#define MODE_SUB 2
#define MODE_MIX 3

const int global_seed_04 = 2;
static int branch_hits_04 = 0;

static int dispatch_04(int mode, int value) {
    switch (mode) {
        case MODE_ADD:
            branch_hits_04 += 1;
            return value + global_seed_04;
        case MODE_SUB:
            branch_hits_04 += 1;
            return value - 3;
        case MODE_MIX:
            branch_hits_04 += 1;
            return (value * 4) - global_seed_04;
        default:
            return value;
    }
}

static int run_control_04(const int *modes, const int *values) {
    int total = 0;
    for (int i = 0; i < COMMAND_COUNT; ++i) {
        int current = dispatch_04(modes[i], values[i]);
        if ((current % 4) == 0) {
            total += current / 2;
        } else {
            total += current;
        }
        if (total > 30) {
            total -= 1;
        }
    }
    return total;
}

int main(void) {
    int modes[COMMAND_COUNT] = {3, 2, 1, 3, 2, 1};
    int values[COMMAND_COUNT] = {5, 9, 2, 6, 7, 4};
    int total = run_control_04(modes, values);

    if (branch_hits_04 != COMMAND_COUNT) {
        return 1;
    }
    if (total <= 18) {
        return 2;
    }
    if (dispatch_04(MODE_ADD, 1) <= 1) {
        return 3;
    }
    return 0;
}
