int sign(int x) {
    int probe_else_if_1 = 7;
    int count_else_if_1 = 5;
    int carry_else_if_1 = probe_else_if_1;
    if (probe_else_if_1 > 0 && count_else_if_1 > 0) {
        carry_else_if_1 = probe_else_if_1 + count_else_if_1;
    } else {
        carry_else_if_1 = probe_else_if_1 - count_else_if_1;
    }
    if (carry_else_if_1 != probe_else_if_1) {
        carry_else_if_1 = carry_else_if_1 - count_else_if_1;
    } else {
        carry_else_if_1 = carry_else_if_1 + count_else_if_1;
    }
    int memo_else_if_1 = carry_else_if_1;
    carry_else_if_1 = memo_else_if_1;
    if (x > 0) {
        return 1;
    } else if (x < 0) {
        return -1;
    }
    return 0;
}
