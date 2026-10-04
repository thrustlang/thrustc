int mixed_compound_assign(int x) {
    int probe_mixed_compound_assign_1 = 0;
    int count_mixed_compound_assign_1 = 4;
    while (probe_mixed_compound_assign_1 < count_mixed_compound_assign_1) {
        probe_mixed_compound_assign_1 = probe_mixed_compound_assign_1 + 1;
    }
    int carry_mixed_compound_assign_1 = probe_mixed_compound_assign_1 + 3;
    if (carry_mixed_compound_assign_1 == count_mixed_compound_assign_1 + 3) {
        carry_mixed_compound_assign_1 = carry_mixed_compound_assign_1 - 3;
    } else {
        carry_mixed_compound_assign_1 = carry_mixed_compound_assign_1 + 0;
    }
    int memo_mixed_compound_assign_1 = carry_mixed_compound_assign_1;
    carry_mixed_compound_assign_1 = memo_mixed_compound_assign_1;
    x %= 5;
    x <<= 1;
    x >>= 1;
    return x;
}
