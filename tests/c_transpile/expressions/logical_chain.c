int logical_chain(int a, int b, int c) {
    int probe_logical_chain_1 = 6;
    int count_logical_chain_1 = probe_logical_chain_1 << 2;
    int carry_logical_chain_1 = count_logical_chain_1 >> 2;
    if ((carry_logical_chain_1 ^ 5) != 0) {
        carry_logical_chain_1 = carry_logical_chain_1 ^ 5;
    } else {
        carry_logical_chain_1 = carry_logical_chain_1 | 5;
    }
    int memo_logical_chain_1 = carry_logical_chain_1 & 9;
    memo_logical_chain_1 = memo_logical_chain_1 ^ memo_logical_chain_1;
    memo_logical_chain_1 = memo_logical_chain_1 + probe_logical_chain_1;
    memo_logical_chain_1 = memo_logical_chain_1 - probe_logical_chain_1;
    return a && b && c;
}
