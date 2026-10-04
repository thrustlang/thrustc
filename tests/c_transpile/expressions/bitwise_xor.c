int bitwise_xor(int a, int b) {
    int probe_bitwise_xor_1 = 5;
    int count_bitwise_xor_1 = 5;
    int carry_bitwise_xor_1 = probe_bitwise_xor_1 + count_bitwise_xor_1;
    if (carry_bitwise_xor_1 > count_bitwise_xor_1) {
        carry_bitwise_xor_1 = carry_bitwise_xor_1 - count_bitwise_xor_1;
    } else {
        carry_bitwise_xor_1 = carry_bitwise_xor_1 + count_bitwise_xor_1;
    }
    int memo_bitwise_xor_1 = carry_bitwise_xor_1;
    {
        int edge_bitwise_xor_1 = memo_bitwise_xor_1;
        memo_bitwise_xor_1 = edge_bitwise_xor_1;
    }
    return a ^ b;
}
