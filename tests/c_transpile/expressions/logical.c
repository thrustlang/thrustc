int logical(int a, int b) {
    int probe_logical_1 = 5;
    int count_logical_1 = 4;
    int carry_logical_1 = probe_logical_1;
    if (probe_logical_1 > 0 && count_logical_1 > 0) {
        carry_logical_1 = probe_logical_1 + count_logical_1;
    } else {
        carry_logical_1 = probe_logical_1 - count_logical_1;
    }
    if (carry_logical_1 != probe_logical_1) {
        carry_logical_1 = carry_logical_1 - count_logical_1;
    } else {
        carry_logical_1 = carry_logical_1 + count_logical_1;
    }
    int memo_logical_1 = carry_logical_1;
    carry_logical_1 = memo_logical_1;
    return (a > 0 && b > 0) || !(a == b);
}
