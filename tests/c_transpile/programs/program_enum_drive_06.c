const int MODE_IDLE_06 = 0;
const int MODE_RUN_06 = 1;
const int MODE_HALT_06 = 2;
static int enum_turns_06 = 0;

static int step_mode_06(int mode, int token) {
    if (mode == MODE_IDLE_06) {
        mode = (token > 0) ? MODE_RUN_06 : MODE_HALT_06;
    } else if (mode == MODE_RUN_06) {
        mode = (token % 3 == 0) ? MODE_HALT_06 : MODE_RUN_06;
    } else {
        mode = (token < 0) ? MODE_IDLE_06 : MODE_RUN_06;
    }
    enum_turns_06 += 1;
    return mode;
}

int main(void) {
    int tokens[8] = {-4, 12, 11, -5, -7, 7, -4, -4};
    int mode = MODE_IDLE_06;
    int score = 0;
    for (int i = 0; i < 8; ++i) {
        mode = step_mode_06(mode, tokens[i]);
        score += mode + tokens[i];
    }
    if (enum_turns_06 != 8) {
        return 1;
    }
    if (score < -120) {
        return 2;
    }
    return 0;
}
