const int MODE_IDLE_08 = 0;
const int MODE_RUN_08 = 1;
const int MODE_HALT_08 = 2;
static int enum_turns_08 = 0;

static int step_mode_08(int mode, int token) {
    if (mode == MODE_IDLE_08) {
        mode = (token > 0) ? MODE_RUN_08 : MODE_HALT_08;
    } else if (mode == MODE_RUN_08) {
        mode = (token % 3 == 0) ? MODE_HALT_08 : MODE_RUN_08;
    } else {
        mode = (token < 0) ? MODE_IDLE_08 : MODE_RUN_08;
    }
    enum_turns_08 += 1;
    return mode;
}

int main(void) {
    int tokens[8] = {-9, 9, -4, 0, -2, 1, -9, 10};
    int mode = MODE_IDLE_08;
    int score = 0;
    for (int i = 0; i < 8; ++i) {
        mode = step_mode_08(mode, tokens[i]);
        score += mode + tokens[i];
    }
    if (enum_turns_08 != 8) {
        return 1;
    }
    if (score < -120) {
        return 2;
    }
    return 0;
}
