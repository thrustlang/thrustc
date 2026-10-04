int clamp_min_zero(int x) {
    int probe_if_only_1 = 5;
    int count_if_only_1 = 3;
    int carry_if_only_1 = probe_if_only_1;
    if (probe_if_only_1 > 0 && count_if_only_1 > 0) {
        carry_if_only_1 = probe_if_only_1 + count_if_only_1;
    } else {
        carry_if_only_1 = probe_if_only_1 - count_if_only_1;
    }
    if (carry_if_only_1 != probe_if_only_1) {
        carry_if_only_1 = carry_if_only_1 - count_if_only_1;
    } else {
        carry_if_only_1 = carry_if_only_1 + count_if_only_1;
    }
    int memo_if_only_1 = carry_if_only_1;
    carry_if_only_1 = memo_if_only_1;
    if (x < 0) {
        x = 0;
    }
    return x;
}
