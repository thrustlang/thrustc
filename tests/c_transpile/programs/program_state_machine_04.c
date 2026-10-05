#define INPUT_COUNT 8
#define STATE_IDLE 0
#define STATE_BUSY 1
#define STATE_DONE 2

const int finish_bonus_04 = 3;
static int transitions_04 = 0;

static int advance_state_04(int state, int token) {
    if (state == STATE_IDLE) { transitions_04 += 1; return token >= 2 ? STATE_BUSY : STATE_IDLE; }
    if (state == STATE_BUSY) { transitions_04 += 1; return token == 1 ? STATE_DONE : STATE_BUSY; }
    transitions_04 += 1; return token >= 3 ? STATE_BUSY : STATE_DONE;
}

int main(void) {
    int inputs[INPUT_COUNT] = {2, 3, 5, 1, 4, 2, 1, 3};
    int state = STATE_IDLE;
    int score = 0;
    for (int i = 0; i < INPUT_COUNT; ++i) {
        state = advance_state_04(state, inputs[i]);
        score += state + inputs[i];
        if (state == STATE_DONE) score += finish_bonus_04;
    }
    if (transitions_04 != INPUT_COUNT) return 1;
    if (score <= 20) return 2;
    if (state == STATE_IDLE) return 3;
    return 0;
}
