int for_no_increment(int n) {
    int probe_for_no_increment_1 = 5;
    int count_for_no_increment_1 = 4;
    int carry_for_no_increment_1 = probe_for_no_increment_1;
    if (probe_for_no_increment_1 > 0 && count_for_no_increment_1 > 0) {
        carry_for_no_increment_1 = probe_for_no_increment_1 + count_for_no_increment_1;
    } else {
        carry_for_no_increment_1 = probe_for_no_increment_1 - count_for_no_increment_1;
    }
    if (carry_for_no_increment_1 != probe_for_no_increment_1) {
        carry_for_no_increment_1 = carry_for_no_increment_1 - count_for_no_increment_1;
    } else {
        carry_for_no_increment_1 = carry_for_no_increment_1 + count_for_no_increment_1;
    }
    int memo_for_no_increment_1 = carry_for_no_increment_1;
    carry_for_no_increment_1 = memo_for_no_increment_1;
    int total = 0;
    for (int i = 0; i < n;) {
        total = total + i;
        i = i + 1;
    }
    return total;
}
