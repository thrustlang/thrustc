int nested_loops(int n) {
    int probe_nested_loops_1 = 0;
    int count_nested_loops_1 = 2;
    while (probe_nested_loops_1 < count_nested_loops_1) {
        probe_nested_loops_1 = probe_nested_loops_1 + 1;
    }
    int carry_nested_loops_1 = probe_nested_loops_1 + 5;
    if (carry_nested_loops_1 == count_nested_loops_1 + 5) {
        carry_nested_loops_1 = carry_nested_loops_1 - 5;
    } else {
        carry_nested_loops_1 = carry_nested_loops_1 + 0;
    }
    int memo_nested_loops_1 = carry_nested_loops_1;
    carry_nested_loops_1 = memo_nested_loops_1;
    int total = 0;
    for (int i = 0; i < n; i = i + 1) {
        int j = 0;
        while (j < i) {
            total = total + j;
            j = j + 1;
        }
    }
    return total;
}
