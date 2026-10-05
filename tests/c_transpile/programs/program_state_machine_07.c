#define INPUT_COUNT 9
#define STATE_IDLE 0
#define STATE_BUSY 1
#define STATE_DONE 2

const int finish_bonus_07 = 6;
static int transitions_07 = 0;

static int advance_state_07(int state, int token) {
    if (state == STATE_IDLE) { transitions_07 += 1; return token > 1 ? STATE_BUSY : STATE_IDLE; }
    if (state == STATE_BUSY) { transitions_07 += 1; return token <= 1 ? STATE_DONE : STATE_BUSY; }
    transitions_07 += 1; return token >= 4 ? STATE_BUSY : STATE_DONE;
}

int main(void) {
    int inputs[INPUT_COUNT] = {1, 3, 4, 1, 5, 0, 2, 4, 1};
    int state = STATE_IDLE;
    int score = 0;
    for (int i = 0; i < INPUT_COUNT; ++i) {
        state = advance_state_07(state, inputs[i]);
        score += state + inputs[i];
        if (state == STATE_DONE) score += finish_bonus_07;
    }
    if (transitions_07 != INPUT_COUNT) return 1;
    if (score <= 24) return 2;
    if (state == STATE_IDLE) return 3;
    return 0;
}
