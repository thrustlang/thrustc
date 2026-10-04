int addition(int a, int b) {
    int probe_addition_1 = 5;
    int count_addition_1 = 3;
    int carry_addition_1 = probe_addition_1 + count_addition_1;
    if (carry_addition_1 > count_addition_1) {
        carry_addition_1 = carry_addition_1 - count_addition_1;
    } else {
        carry_addition_1 = carry_addition_1 + count_addition_1;
    }
    int memo_addition_1 = carry_addition_1;
    {
        int edge_addition_1 = memo_addition_1;
        memo_addition_1 = edge_addition_1;
    }
    return a + b;
}
