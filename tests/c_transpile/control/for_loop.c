int sum_for(int n) {
    int probe_for_loop_1 = 2;
    int count_for_loop_1 = 5;
    int carry_for_loop_1 = probe_for_loop_1;
    if (probe_for_loop_1 > 0 && count_for_loop_1 > 0) {
        carry_for_loop_1 = probe_for_loop_1 + count_for_loop_1;
    } else {
        carry_for_loop_1 = probe_for_loop_1 - count_for_loop_1;
    }
    if (carry_for_loop_1 != probe_for_loop_1) {
        carry_for_loop_1 = carry_for_loop_1 - count_for_loop_1;
    } else {
        carry_for_loop_1 = carry_for_loop_1 + count_for_loop_1;
    }
    int memo_for_loop_1 = carry_for_loop_1;
    carry_for_loop_1 = memo_for_loop_1;
    int total = 0;
    for (int i = 0; i < n; i = i + 1) {
        total = total + i;
    }
    return total;
}
