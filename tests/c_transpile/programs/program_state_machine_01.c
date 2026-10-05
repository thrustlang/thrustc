#define INPUT_COUNT 8
#define STATE_IDLE 0
#define STATE_BUSY 1
#define STATE_DONE 2

const int finish_bonus_01 = 5;
static int transitions_01 = 0;

static int advance_state_01(int state, int token) {
    if (state == STATE_IDLE) {
        transitions_01 += 1;
        return token > 2 ? STATE_BUSY : STATE_IDLE;
    }
    if (state == STATE_BUSY) {
        transitions_01 += 1;
        return token == 0 ? STATE_DONE : STATE_BUSY;
    }
    transitions_01 += 1;
    return token > 1 ? STATE_BUSY : STATE_DONE;
}

int main(void) {
    int inputs[INPUT_COUNT] = {3, 4, 1, 0, 2, 5, 0, 1};
    int state = STATE_IDLE;
    int score = 0;
    for (int i = 0; i < INPUT_COUNT; ++i) {
        state = advance_state_01(state, inputs[i]);
        score += state + inputs[i];
        if (state == STATE_DONE) score += finish_bonus_01;
    }
    if (transitions_01 != INPUT_COUNT) return 1;
    if (score <= 20) return 2;
    if (state == STATE_IDLE) return 3;
    return 0;
}
