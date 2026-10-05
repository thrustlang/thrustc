#define INPUT_COUNT 7
#define STATE_IDLE 0
#define STATE_BUSY 1
#define STATE_DONE 2

const int finish_bonus_02 = 4;
static int transitions_02 = 0;

static int advance_state_02(int state, int token) {
    if (state == STATE_IDLE) { transitions_02 += 1; return token >= 3 ? STATE_BUSY : STATE_IDLE; }
    if (state == STATE_BUSY) { transitions_02 += 1; return token <= 1 ? STATE_DONE : STATE_BUSY; }
    transitions_02 += 1; return token >= 4 ? STATE_BUSY : STATE_DONE;
}

int main(void) {
    int inputs[INPUT_COUNT] = {1, 3, 5, 1, 4, 0, 2};
    int state = STATE_IDLE;
    int score = 0;
    for (int i = 0; i < INPUT_COUNT; ++i) {
        state = advance_state_02(state, inputs[i]);
        score += state + inputs[i];
        if (state == STATE_DONE) score += finish_bonus_02;
    }
    if (transitions_02 != INPUT_COUNT) return 1;
    if (score <= 18) return 2;
    if (state == STATE_IDLE) return 3;
    return 0;
}
