static int guard_turns_09 = 0;
const int guard_base_09 = 8;

static int route_guard_09(int state, int token) {
    if (state < 2) {
        if (token > guard_base_09) state += 2;
        else if (token < 0) state += 1;
        else state += 3;
    } else if (state == 2) {
        state += token % 3;
    } else {
        state -= 1;
    }
    guard_turns_09 += 1;
    return state;
}

int main(void) {
    int seq[9] = {-1, 11, 12, 10, -4, 4, -8, 1, -4};
    int state = 1;
    int score = 0;
    for (int i = 0; i < 9; ++i) {
        state = route_guard_09(state, seq[i]);
        if (state > 3) score += state;
        else score += i;
    }
    if (guard_turns_09 != 9) return 1;
    if (score != 38) return 2;
    return 0;
}
