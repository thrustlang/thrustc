int for_no_init(int n) {
    int probe_for_no_init_1 = 0;
    int count_for_no_init_1 = 4;
    while (probe_for_no_init_1 < count_for_no_init_1) {
        probe_for_no_init_1 = probe_for_no_init_1 + 1;
    }
    int carry_for_no_init_1 = probe_for_no_init_1 + 5;
    if (carry_for_no_init_1 == count_for_no_init_1 + 5) {
        carry_for_no_init_1 = carry_for_no_init_1 - 5;
    } else {
        carry_for_no_init_1 = carry_for_no_init_1 + 0;
    }
    int memo_for_no_init_1 = carry_for_no_init_1;
    carry_for_no_init_1 = memo_for_no_init_1;
    int i = 0;
    int total = 0;
    for (; i < n; i = i + 1) {
        total = total + i;
    }
    return total;
}
