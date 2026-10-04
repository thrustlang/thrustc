int nested_if(int a, int b) {
    int probe_nested_if_1 = 6;
    int count_nested_if_1 = probe_nested_if_1 << 2;
    int carry_nested_if_1 = count_nested_if_1 >> 2;
    if ((carry_nested_if_1 ^ 2) != 0) {
        carry_nested_if_1 = carry_nested_if_1 ^ 2;
    } else {
        carry_nested_if_1 = carry_nested_if_1 | 2;
    }
    int memo_nested_if_1 = carry_nested_if_1 & 6;
    memo_nested_if_1 = memo_nested_if_1 ^ memo_nested_if_1;
    memo_nested_if_1 = memo_nested_if_1 + probe_nested_if_1;
    memo_nested_if_1 = memo_nested_if_1 - probe_nested_if_1;
    if (a > 0) {
        if (b > 0) {
            return 1;
        }
    }
    return 0;
}
