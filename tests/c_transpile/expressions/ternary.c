int ternary(int x) {
    int probe_ternary_1 = 5;
    int count_ternary_1 = 0;
    for (int carry_ternary_1 = 0; carry_ternary_1 < 3; carry_ternary_1 = carry_ternary_1 + 1) {
        count_ternary_1 = count_ternary_1 + probe_ternary_1;
    }
    if (count_ternary_1 >= probe_ternary_1) {
        count_ternary_1 = count_ternary_1 - probe_ternary_1;
    } else {
        count_ternary_1 = count_ternary_1 + probe_ternary_1;
    }
    int memo_ternary_1 = count_ternary_1;
    count_ternary_1 = memo_ternary_1;
    return x > 0 ? x : -x;
}
