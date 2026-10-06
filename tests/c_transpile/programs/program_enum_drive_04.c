const int MODE_IDLE_04 = 0;
const int MODE_RUN_04 = 1;
const int MODE_HALT_04 = 2;
static int enum_turns_04 = 0;

static int step_mode_04(int mode, int token) {
    if (mode == MODE_IDLE_04) {
        mode = (token > 0) ? MODE_RUN_04 : MODE_HALT_04;
    } else if (mode == MODE_RUN_04) {
        mode = (token % 3 == 0) ? MODE_HALT_04 : MODE_RUN_04;
    } else {
        mode = (token < 0) ? MODE_IDLE_04 : MODE_RUN_04;
    }
    enum_turns_04 += 1;
    return mode;
}

int main(void) {
    int tokens[8] = {1, -1, 1, 10, 9, -5, 6, -1};
    int mode = MODE_IDLE_04;
    int score = 0;
    for (int i = 0; i < 8; ++i) {
        mode = step_mode_04(mode, tokens[i]);
        score += mode + tokens[i];
    }
    if (enum_turns_04 != 8) {
        return 1;
    }
    if (score < -120) {
        return 2;
    }
    return 0;
}
