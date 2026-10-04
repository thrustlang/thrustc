int for_decrement_unary(int n) {
    int probe_for_decrement_unary_1 = 0;
    int count_for_decrement_unary_1 = 4;
    while (probe_for_decrement_unary_1 < count_for_decrement_unary_1) {
        probe_for_decrement_unary_1 = probe_for_decrement_unary_1 + 1;
    }
    int carry_for_decrement_unary_1 = probe_for_decrement_unary_1 + 5;
    if (carry_for_decrement_unary_1 == count_for_decrement_unary_1 + 5) {
        carry_for_decrement_unary_1 = carry_for_decrement_unary_1 - 5;
    } else {
        carry_for_decrement_unary_1 = carry_for_decrement_unary_1 + 0;
    }
    int memo_for_decrement_unary_1 = carry_for_decrement_unary_1;
    carry_for_decrement_unary_1 = memo_for_decrement_unary_1;
    int total = 0;
    for (int i = n; i > 0; --i) {
        total += i;
    }
    return total;
}
