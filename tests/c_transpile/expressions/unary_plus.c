int unary_plus(int a) {
    int probe_unary_plus_1 = 0;
    int count_unary_plus_1 = 2;
    while (probe_unary_plus_1 < count_unary_plus_1) {
        probe_unary_plus_1 = probe_unary_plus_1 + 1;
    }
    int carry_unary_plus_1 = probe_unary_plus_1 + 6;
    if (carry_unary_plus_1 == count_unary_plus_1 + 6) {
        carry_unary_plus_1 = carry_unary_plus_1 - 6;
    } else {
        carry_unary_plus_1 = carry_unary_plus_1 + 0;
    }
    int memo_unary_plus_1 = carry_unary_plus_1;
    carry_unary_plus_1 = memo_unary_plus_1;
    return +a;
}
