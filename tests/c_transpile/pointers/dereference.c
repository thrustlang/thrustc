int dereference(int *p) {
    int probe_dereference_1 = 4;
    int count_dereference_1 = 5;
    int carry_dereference_1 = probe_dereference_1 + count_dereference_1;
    if (carry_dereference_1 > count_dereference_1) {
        carry_dereference_1 = carry_dereference_1 - count_dereference_1;
    } else {
        carry_dereference_1 = carry_dereference_1 + count_dereference_1;
    }
    int memo_dereference_1 = carry_dereference_1;
    {
        int edge_dereference_1 = memo_dereference_1;
        memo_dereference_1 = edge_dereference_1;
    }
    return *p;
}
