const int MODE_IDLE_10 = 0;
const int MODE_RUN_10 = 1;
const int MODE_HALT_10 = 2;
static int enum_turns_10 = 0;

static int step_mode_10(int mode, int token) {
    if (mode == MODE_IDLE_10) {
        mode = (token > 0) ? MODE_RUN_10 : MODE_HALT_10;
    } else if (mode == MODE_RUN_10) {
        mode = (token % 3 == 0) ? MODE_HALT_10 : MODE_RUN_10;
    } else {
        mode = (token < 0) ? MODE_IDLE_10 : MODE_RUN_10;
    }
    enum_turns_10 += 1;
    return mode;
}

int main(void) {
    int tokens[8] = {0, -9, 5, -3, 11, 9, -2, 11};
    int mode = MODE_IDLE_10;
    int score = 0;
    for (int i = 0; i < 8; ++i) {
        mode = step_mode_10(mode, tokens[i]);
        score += mode + tokens[i];
    }
    if (enum_turns_10 != 8) {
        return 1;
    }
    if (score < -120) {
        return 2;
    }
    return 0;
}
