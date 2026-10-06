const int MODE_IDLE_05 = 0;
const int MODE_RUN_05 = 1;
const int MODE_HALT_05 = 2;
static int enum_turns_05 = 0;

static int step_mode_05(int mode, int token) {
    if (mode == MODE_IDLE_05) {
        mode = (token > 0) ? MODE_RUN_05 : MODE_HALT_05;
    } else if (mode == MODE_RUN_05) {
        mode = (token % 3 == 0) ? MODE_HALT_05 : MODE_RUN_05;
    } else {
        mode = (token < 0) ? MODE_IDLE_05 : MODE_RUN_05;
    }
    enum_turns_05 += 1;
    return mode;
}

int main(void) {
    int tokens[8] = {-3, -3, -8, -3, 5, 6, 1, -1};
    int mode = MODE_IDLE_05;
    int score = 0;
    for (int i = 0; i < 8; ++i) {
        mode = step_mode_05(mode, tokens[i]);
        score += mode + tokens[i];
    }
    if (enum_turns_05 != 8) {
        return 1;
    }
    if (score < -120) {
        return 2;
    }
    return 0;
}
