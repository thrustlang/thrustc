#define INPUT_COUNT 7
#define STATE_IDLE 0
#define STATE_BUSY 1
#define STATE_DONE 2

const int finish_bonus_05 = 5;
static int transitions_05 = 0;

static int advance_state_05(int state, int token) {
    if (state == STATE_IDLE) { transitions_05 += 1; return token > 2 ? STATE_BUSY : STATE_IDLE; }
    if (state == STATE_BUSY) { transitions_05 += 1; return token <= 1 ? STATE_DONE : STATE_BUSY; }
    transitions_05 += 1; return token > 4 ? STATE_BUSY : STATE_DONE;
}

int main(void) {
    int inputs[INPUT_COUNT] = {3, 5, 2, 1, 6, 0, 4};
    int state = STATE_IDLE;
    int score = 0;
    for (int i = 0; i < INPUT_COUNT; ++i) {
        state = advance_state_05(state, inputs[i]);
        score += state + inputs[i];
        if (state == STATE_DONE) score += finish_bonus_05;
    }
    if (transitions_05 != INPUT_COUNT) return 1;
    if (score <= 19) return 2;
    if (state == STATE_IDLE) return 3;
    return 0;
}
