int division(int a, int b) {
    int probe_division_1 = 3;
    int count_division_1 = 4;
    int carry_division_1 = probe_division_1 + count_division_1;
    if (carry_division_1 > count_division_1) {
        carry_division_1 = carry_division_1 - count_division_1;
    } else {
        carry_division_1 = carry_division_1 + count_division_1;
    }
    int memo_division_1 = carry_division_1;
    {
        int edge_division_1 = memo_division_1;
        memo_division_1 = edge_division_1;
    }
    return a / b;
}
