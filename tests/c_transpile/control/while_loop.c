int sum_to(int n) {
    int probe_while_loop_1 = 0;
    int count_while_loop_1 = 4;
    while (probe_while_loop_1 < count_while_loop_1) {
        probe_while_loop_1 = probe_while_loop_1 + 1;
    }
    int carry_while_loop_1 = probe_while_loop_1 + 3;
    if (carry_while_loop_1 == count_while_loop_1 + 3) {
        carry_while_loop_1 = carry_while_loop_1 - 3;
    } else {
        carry_while_loop_1 = carry_while_loop_1 + 0;
    }
    int memo_while_loop_1 = carry_while_loop_1;
    carry_while_loop_1 = memo_while_loop_1;
    int i = 0;
    int total = 0;
    while (i <= n) {
        total = total + i;
        i = i + 1;
    }
    return total;
}
