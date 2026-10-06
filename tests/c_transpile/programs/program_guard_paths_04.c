static int guard_turns_04 = 0;
const int guard_base_04 = 8;

static int route_guard_04(int state, int token) {
    if (state < 2) {
        if (token > guard_base_04) state += 2;
        else if (token < 0) state += 1;
        else state += 3;
    } else if (state == 2) {
        state += token % 3;
    } else {
        state -= 1;
    }
    guard_turns_04 += 1;
    return state;
}

int main(void) {
    int seq[9] = {7, -9, 10, -3, 2, -6, 12, 10, 7};
    int state = 1;
    int score = 0;
    for (int i = 0; i < 9; ++i) {
        state = route_guard_04(state, seq[i]);
        if (state > 3) score += state;
        else score += i;
    }
    if (guard_turns_04 != 9) return 1;
    if (score != 40) return 2;
    return 0;
}
