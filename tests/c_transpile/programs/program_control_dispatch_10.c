#define COMMAND_COUNT 7
#define MODE_ADD 1
#define MODE_SUB 2
#define MODE_MIX 3

const int global_seed_10 = 4;
static int branch_hits_10 = 0;

static int dispatch_10(int mode, int value) {
    switch (mode) {
        case MODE_ADD:
            branch_hits_10 += 1;
            return value + global_seed_10;
        case MODE_SUB:
            branch_hits_10 += 1;
            return value - 2;
        case MODE_MIX:
            branch_hits_10 += 1;
            return (value * 2) + 2;
        default:
            return value;
    }
}

static int run_control_10(const int *modes, const int *values) {
    int total = 0;
    for (int i = 0; i < COMMAND_COUNT; ++i) {
        int current = dispatch_10(modes[i], values[i]);
        if ((current & 1) == 0) {
            total += current / 2;
        } else {
            total += current;
        }
        if (total > 34) {
            total -= 1;
        }
    }
    return total;
}

int main(void) {
    int modes[COMMAND_COUNT] = {1, 2, 3, 1, 2, 3, 1};
    int values[COMMAND_COUNT] = {6, 8, 5, 4, 7, 3, 9};
    int total = run_control_10(modes, values);

    if (branch_hits_10 != COMMAND_COUNT) {
        return 1;
    }
    if (total <= 17) {
        return 2;
    }
    if (dispatch_10(MODE_MIX, 2) <= 2) {
        return 3;
    }
    return 0;
}
