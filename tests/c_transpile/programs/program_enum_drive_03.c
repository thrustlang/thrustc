const int MODE_IDLE_03 = 0;
const int MODE_RUN_03 = 1;
const int MODE_HALT_03 = 2;
static int enum_turns_03 = 0;

static int step_mode_03(int mode, int token) {
    if (mode == MODE_IDLE_03) {
        mode = (token > 0) ? MODE_RUN_03 : MODE_HALT_03;
    } else if (mode == MODE_RUN_03) {
        mode = (token % 3 == 0) ? MODE_HALT_03 : MODE_RUN_03;
    } else {
        mode = (token < 0) ? MODE_IDLE_03 : MODE_RUN_03;
    }
    enum_turns_03 += 1;
    return mode;
}

int main(void) {
    int tokens[8] = {-4, 3, 0, 8, 3, 9, 9, 5};
    int mode = MODE_IDLE_03;
    int score = 0;
    for (int i = 0; i < 8; ++i) {
        mode = step_mode_03(mode, tokens[i]);
        score += mode + tokens[i];
    }
    if (enum_turns_03 != 8) {
        return 1;
    }
    if (score < -120) {
        return 2;
    }
    return 0;
}
