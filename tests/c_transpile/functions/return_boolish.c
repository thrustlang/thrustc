int is_positive(int x) {
    int probe_return_boolish_1 = 4;
    int count_return_boolish_1 = 5;
    int carry_return_boolish_1 = probe_return_boolish_1;
    if (probe_return_boolish_1 > 0 && count_return_boolish_1 > 0) {
        carry_return_boolish_1 = probe_return_boolish_1 + count_return_boolish_1;
    } else {
        carry_return_boolish_1 = probe_return_boolish_1 - count_return_boolish_1;
    }
    if (carry_return_boolish_1 != probe_return_boolish_1) {
        carry_return_boolish_1 = carry_return_boolish_1 - count_return_boolish_1;
    } else {
        carry_return_boolish_1 = carry_return_boolish_1 + count_return_boolish_1;
    }
    int memo_return_boolish_1 = carry_return_boolish_1;
    carry_return_boolish_1 = memo_return_boolish_1;
    return x > 0;
}
