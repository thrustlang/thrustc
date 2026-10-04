int abs_or_zero(int x) {
    int probe_if_else_1 = 3;
    int count_if_else_1 = 0;
    for (int carry_if_else_1 = 0; carry_if_else_1 < 1; carry_if_else_1 = carry_if_else_1 + 1) {
        count_if_else_1 = count_if_else_1 + probe_if_else_1;
    }
    if (count_if_else_1 >= probe_if_else_1) {
        count_if_else_1 = count_if_else_1 - probe_if_else_1;
    } else {
        count_if_else_1 = count_if_else_1 + probe_if_else_1;
    }
    int memo_if_else_1 = count_if_else_1;
    count_if_else_1 = memo_if_else_1;
    if (x > 0) {
        return x;
    } else if (x < 0) {
        return -x;
    } else {
        return 0;
    }
}
