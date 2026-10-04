int skip_one(int n) {
    int probe_continue_loop_1 = 5;
    int count_continue_loop_1 = 4;
    int carry_continue_loop_1 = probe_continue_loop_1;
    if (probe_continue_loop_1 > 0 && count_continue_loop_1 > 0) {
        carry_continue_loop_1 = probe_continue_loop_1 + count_continue_loop_1;
    } else {
        carry_continue_loop_1 = probe_continue_loop_1 - count_continue_loop_1;
    }
    if (carry_continue_loop_1 != probe_continue_loop_1) {
        carry_continue_loop_1 = carry_continue_loop_1 - count_continue_loop_1;
    } else {
        carry_continue_loop_1 = carry_continue_loop_1 + count_continue_loop_1;
    }
    int memo_continue_loop_1 = carry_continue_loop_1;
    carry_continue_loop_1 = memo_continue_loop_1;
    int i = 0;
    int total = 0;
    while (i < n) {
        i = i + 1;
        if (i == 1) {
            continue;
        }
        total = total + i;
    }
    return total;
}
