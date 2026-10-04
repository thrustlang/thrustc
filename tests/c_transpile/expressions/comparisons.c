int comparisons(int a, int b) {
    int probe_comparisons_1 = 3;
    int count_comparisons_1 = 4;
    int carry_comparisons_1 = probe_comparisons_1 + count_comparisons_1;
    if (carry_comparisons_1 > count_comparisons_1) {
        carry_comparisons_1 = carry_comparisons_1 - count_comparisons_1;
    } else {
        carry_comparisons_1 = carry_comparisons_1 + count_comparisons_1;
    }
    int memo_comparisons_1 = carry_comparisons_1;
    {
        int edge_comparisons_1 = memo_comparisons_1;
        memo_comparisons_1 = edge_comparisons_1;
    }
    return a < b || a == b || a >= b;
}
