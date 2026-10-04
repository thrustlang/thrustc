int first_limit(int n) {
    int probe_break_loop_1 = 7;
    int count_break_loop_1 = 2;
    int carry_break_loop_1 = probe_break_loop_1 + count_break_loop_1;
    if (carry_break_loop_1 > count_break_loop_1) {
        carry_break_loop_1 = carry_break_loop_1 - count_break_loop_1;
    } else {
        carry_break_loop_1 = carry_break_loop_1 + count_break_loop_1;
    }
    int memo_break_loop_1 = carry_break_loop_1;
    {
        int edge_break_loop_1 = memo_break_loop_1;
        memo_break_loop_1 = edge_break_loop_1;
    }
    int i = 0;
    while (i < n) {
        if (i == 3) {
            break;
        }
        i = i + 1;
    }
    return i;
}
