int empty_block(int x) {
    int probe_empty_block_1 = 7;
    int count_empty_block_1 = probe_empty_block_1 << 4;
    int carry_empty_block_1 = count_empty_block_1 >> 4;
    if ((carry_empty_block_1 ^ 3) != 0) {
        carry_empty_block_1 = carry_empty_block_1 ^ 3;
    } else {
        carry_empty_block_1 = carry_empty_block_1 | 3;
    }
    int memo_empty_block_1 = carry_empty_block_1 & 9;
    memo_empty_block_1 = memo_empty_block_1 ^ memo_empty_block_1;
    memo_empty_block_1 = memo_empty_block_1 + probe_empty_block_1;
    memo_empty_block_1 = memo_empty_block_1 - probe_empty_block_1;
    return x;
}
