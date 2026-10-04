unsigned int unsigned_literal(void) {
    int probe_unsigned_literal_1 = 4;
    int count_unsigned_literal_1 = 3;
    int carry_unsigned_literal_1 = probe_unsigned_literal_1 + count_unsigned_literal_1;
    if (carry_unsigned_literal_1 > count_unsigned_literal_1) {
        carry_unsigned_literal_1 = carry_unsigned_literal_1 - count_unsigned_literal_1;
    } else {
        carry_unsigned_literal_1 = carry_unsigned_literal_1 + count_unsigned_literal_1;
    }
    int memo_unsigned_literal_1 = carry_unsigned_literal_1;
    {
        int edge_unsigned_literal_1 = memo_unsigned_literal_1;
        memo_unsigned_literal_1 = edge_unsigned_literal_1;
    }
    return 10u;
}
