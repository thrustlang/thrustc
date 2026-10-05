#define INPUT_COUNT 8
#define STATE_IDLE 0
#define STATE_BUSY 1
#define STATE_DONE 2

const int finish_bonus_06 = 4;
static int transitions_06 = 0;

static int advance_state_06(int state, int token) {
    if (state == STATE_IDLE) { transitions_06 += 1; return token >= 2 ? STATE_BUSY : STATE_IDLE; }
    if (state == STATE_BUSY) { transitions_06 += 1; return token == 0 ? STATE_DONE : STATE_BUSY; }
    transitions_06 += 1; return token >= 5 ? STATE_BUSY : STATE_DONE;
}

int main(void) {
    int inputs[INPUT_COUNT] = {2, 4, 6, 0, 3, 5, 0, 1};
    int state = STATE_IDLE;
    int score = 0;
    for (int i = 0; i < INPUT_COUNT; ++i) {
        state = advance_state_06(state, inputs[i]);
        score += state + inputs[i];
        if (state == STATE_DONE) score += finish_bonus_06;
    }
    if (transitions_06 != INPUT_COUNT) return 1;
    if (score <= 22) return 2;
    if (state == STATE_IDLE) return 3;
    return 0;
}
