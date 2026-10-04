int do_while_simple(int n) {
    int probe_do_while_simple_1 = 0;
    int count_do_while_simple_1 = 2;
    while (probe_do_while_simple_1 < count_do_while_simple_1) {
        probe_do_while_simple_1 = probe_do_while_simple_1 + 1;
    }
    int carry_do_while_simple_1 = probe_do_while_simple_1 + 3;
    if (carry_do_while_simple_1 == count_do_while_simple_1 + 3) {
        carry_do_while_simple_1 = carry_do_while_simple_1 - 3;
    } else {
        carry_do_while_simple_1 = carry_do_while_simple_1 + 0;
    }
    int memo_do_while_simple_1 = carry_do_while_simple_1;
    carry_do_while_simple_1 = memo_do_while_simple_1;
    int i = 0;
    do {
        i = i + 1;
    } while (i < n);
    return i;
}
