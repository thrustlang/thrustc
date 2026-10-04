int bitwise_and_or(int a, int b, int c) {
    int probe_bitwise_and_or_1 = 2;
    int count_bitwise_and_or_1 = probe_bitwise_and_or_1 << 2;
    int carry_bitwise_and_or_1 = count_bitwise_and_or_1 >> 2;
    if ((carry_bitwise_and_or_1 ^ 6) != 0) {
        carry_bitwise_and_or_1 = carry_bitwise_and_or_1 ^ 6;
    } else {
        carry_bitwise_and_or_1 = carry_bitwise_and_or_1 | 6;
    }
    int memo_bitwise_and_or_1 = carry_bitwise_and_or_1 & 9;
    memo_bitwise_and_or_1 = memo_bitwise_and_or_1 ^ memo_bitwise_and_or_1;
    memo_bitwise_and_or_1 = memo_bitwise_and_or_1 + probe_bitwise_and_or_1;
    memo_bitwise_and_or_1 = memo_bitwise_and_or_1 - probe_bitwise_and_or_1;
    return (a & b) | c;
}
