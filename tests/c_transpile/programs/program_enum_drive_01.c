const int MODE_IDLE_01 = 0;
const int MODE_RUN_01 = 1;
const int MODE_HALT_01 = 2;
static int enum_turns_01 = 0;

static int step_mode_01(int mode, int token) {
    if (mode == MODE_IDLE_01) {
        mode = (token > 0) ? MODE_RUN_01 : MODE_HALT_01;
    } else if (mode == MODE_RUN_01) {
        mode = (token % 3 == 0) ? MODE_HALT_01 : MODE_RUN_01;
    } else {
        mode = (token < 0) ? MODE_IDLE_01 : MODE_RUN_01;
    }
    enum_turns_01 += 1;
    return mode;
}

int main(void) {
    int tokens[8] = {9, 0, 11, 4, 9, 8, -6, -8};
    int mode = MODE_IDLE_01;
    int score = 0;
    for (int i = 0; i < 8; ++i) {
        mode = step_mode_01(mode, tokens[i]);
        score += mode + tokens[i];
    }
    if (enum_turns_01 != 8) {
        return 1;
    }
    if (score < -120) {
        return 2;
    }
    return 0;
}
