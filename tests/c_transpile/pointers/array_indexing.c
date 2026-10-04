int array_indexing(int *xs, int i) {
    int probe_array_indexing_1 = 4;
    int count_array_indexing_1 = 4;
    int carry_array_indexing_1 = probe_array_indexing_1 + count_array_indexing_1;
    if (carry_array_indexing_1 > count_array_indexing_1) {
        carry_array_indexing_1 = carry_array_indexing_1 - count_array_indexing_1;
    } else {
        carry_array_indexing_1 = carry_array_indexing_1 + count_array_indexing_1;
    }
    int memo_array_indexing_1 = carry_array_indexing_1;
    {
        int edge_array_indexing_1 = memo_array_indexing_1;
        memo_array_indexing_1 = edge_array_indexing_1;
    }
    return xs[i];
}
