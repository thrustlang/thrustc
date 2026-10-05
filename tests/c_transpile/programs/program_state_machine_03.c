#define INPUT_COUNT 9
#define STATE_IDLE 0
#define STATE_BUSY 1
#define STATE_DONE 2

const int finish_bonus_03 = 6;
static int transitions_03 = 0;

static int advance_state_03(int state, int token) {
    if (state == STATE_IDLE) { transitions_03 += 1; return token > 1 ? STATE_BUSY : STATE_IDLE; }
    if (state == STATE_BUSY) { transitions_03 += 1; return token == 0 ? STATE_DONE : STATE_BUSY; }
    transitions_03 += 1; return token > 3 ? STATE_BUSY : STATE_DONE;
}

int main(void) {
    int inputs[INPUT_COUNT] = {2, 4, 3, 0, 5, 1, 0, 4, 2};
    int state = STATE_IDLE;
    int score = 0;
    for (int i = 0; i < INPUT_COUNT; ++i) {
        state = advance_state_03(state, inputs[i]);
        score += state + inputs[i];
        if (state == STATE_DONE) score += finish_bonus_03;
    }
    if (transitions_03 != INPUT_COUNT) return 1;
    if (score <= 24) return 2;
    if (state == STATE_IDLE) return 3;
    return 0;
}
