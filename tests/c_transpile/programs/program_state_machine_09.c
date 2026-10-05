#define INPUT_COUNT 8
#define STATE_IDLE 0
#define STATE_BUSY 1
#define STATE_DONE 2

const int finish_bonus_09 = 5;
static int transitions_09 = 0;

static int advance_state_09(int state, int token) {
    if (state == STATE_IDLE) { transitions_09 += 1; return token >= 3 ? STATE_BUSY : STATE_IDLE; }
    if (state == STATE_BUSY) { transitions_09 += 1; return token == 0 ? STATE_DONE : STATE_BUSY; }
    transitions_09 += 1; return token >= 3 ? STATE_BUSY : STATE_DONE;
}

int main(void) {
    int inputs[INPUT_COUNT] = {3, 6, 1, 0, 4, 7, 0, 2};
    int state = STATE_IDLE;
    int score = 0;
    for (int i = 0; i < INPUT_COUNT; ++i) {
        state = advance_state_09(state, inputs[i]);
        score += state + inputs[i];
        if (state == STATE_DONE) score += finish_bonus_09;
    }
    if (transitions_09 != INPUT_COUNT) return 1;
    if (score <= 21) return 2;
    if (state == STATE_IDLE) return 3;
    return 0;
}
