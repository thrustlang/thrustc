const int MODE_IDLE_02 = 0;
const int MODE_RUN_02 = 1;
const int MODE_HALT_02 = 2;
static int enum_turns_02 = 0;

static int step_mode_02(int mode, int token) {
    if (mode == MODE_IDLE_02) {
        mode = (token > 0) ? MODE_RUN_02 : MODE_HALT_02;
    } else if (mode == MODE_RUN_02) {
        mode = (token % 3 == 0) ? MODE_HALT_02 : MODE_RUN_02;
    } else {
        mode = (token < 0) ? MODE_IDLE_02 : MODE_RUN_02;
    }
    enum_turns_02 += 1;
    return mode;
}

int main(void) {
    int tokens[8] = {11, -1, -8, -9, 10, 3, 5, -1};
    int mode = MODE_IDLE_02;
    int score = 0;
    for (int i = 0; i < 8; ++i) {
        mode = step_mode_02(mode, tokens[i]);
        score += mode + tokens[i];
    }
    if (enum_turns_02 != 8) {
        return 1;
    }
    if (score < -120) {
        return 2;
    }
    return 0;
}
