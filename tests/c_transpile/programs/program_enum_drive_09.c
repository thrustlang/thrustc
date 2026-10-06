const int MODE_IDLE_09 = 0;
const int MODE_RUN_09 = 1;
const int MODE_HALT_09 = 2;
static int enum_turns_09 = 0;

static int step_mode_09(int mode, int token) {
    if (mode == MODE_IDLE_09) {
        mode = (token > 0) ? MODE_RUN_09 : MODE_HALT_09;
    } else if (mode == MODE_RUN_09) {
        mode = (token % 3 == 0) ? MODE_HALT_09 : MODE_RUN_09;
    } else {
        mode = (token < 0) ? MODE_IDLE_09 : MODE_RUN_09;
    }
    enum_turns_09 += 1;
    return mode;
}

int main(void) {
    int tokens[8] = {8, 1, -2, 7, 8, 2, -3, -1};
    int mode = MODE_IDLE_09;
    int score = 0;
    for (int i = 0; i < 8; ++i) {
        mode = step_mode_09(mode, tokens[i]);
        score += mode + tokens[i];
    }
    if (enum_turns_09 != 8) {
        return 1;
    }
    if (score < -120) {
        return 2;
    }
    return 0;
}
