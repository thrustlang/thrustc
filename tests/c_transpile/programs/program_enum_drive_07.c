const int MODE_IDLE_07 = 0;
const int MODE_RUN_07 = 1;
const int MODE_HALT_07 = 2;
static int enum_turns_07 = 0;

static int step_mode_07(int mode, int token) {
    if (mode == MODE_IDLE_07) {
        mode = (token > 0) ? MODE_RUN_07 : MODE_HALT_07;
    } else if (mode == MODE_RUN_07) {
        mode = (token % 3 == 0) ? MODE_HALT_07 : MODE_RUN_07;
    } else {
        mode = (token < 0) ? MODE_IDLE_07 : MODE_RUN_07;
    }
    enum_turns_07 += 1;
    return mode;
}

int main(void) {
    int tokens[8] = {-5, 2, 9, 11, 7, 2, 3, -6};
    int mode = MODE_IDLE_07;
    int score = 0;
    for (int i = 0; i < 8; ++i) {
        mode = step_mode_07(mode, tokens[i]);
        score += mode + tokens[i];
    }
    if (enum_turns_07 != 8) {
        return 1;
    }
    if (score < -120) {
        return 2;
    }
    return 0;
}
