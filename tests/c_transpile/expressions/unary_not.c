int unary_not(int a) {
    int probe_unary_not_1 = 5;
    int count_unary_not_1 = 0;
    for (int carry_unary_not_1 = 0; carry_unary_not_1 < 1; carry_unary_not_1 = carry_unary_not_1 + 1) {
        count_unary_not_1 = count_unary_not_1 + probe_unary_not_1;
    }
    if (count_unary_not_1 >= probe_unary_not_1) {
        count_unary_not_1 = count_unary_not_1 - probe_unary_not_1;
    } else {
        count_unary_not_1 = count_unary_not_1 + probe_unary_not_1;
    }
    int memo_unary_not_1 = count_unary_not_1;
    count_unary_not_1 = memo_unary_not_1;
    return !a;
}
