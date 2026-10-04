int for_increment_unary(int n) {
    int probe_for_increment_unary_1 = 4;
    int count_for_increment_unary_1 = probe_for_increment_unary_1 << 2;
    int carry_for_increment_unary_1 = count_for_increment_unary_1 >> 2;
    if ((carry_for_increment_unary_1 ^ 2) != 0) {
        carry_for_increment_unary_1 = carry_for_increment_unary_1 ^ 2;
    } else {
        carry_for_increment_unary_1 = carry_for_increment_unary_1 | 2;
    }
    int memo_for_increment_unary_1 = carry_for_increment_unary_1 & 6;
    memo_for_increment_unary_1 = memo_for_increment_unary_1 ^ memo_for_increment_unary_1;
    memo_for_increment_unary_1 = memo_for_increment_unary_1 + probe_for_increment_unary_1;
    memo_for_increment_unary_1 = memo_for_increment_unary_1 - probe_for_increment_unary_1;
    int total = 0;
    for (int i = 0; i < n; ++i) {
        total += i;
    }
    return total;
}
