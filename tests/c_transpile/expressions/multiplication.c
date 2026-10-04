int multiplication(int a, int b) {
    int probe_multiplication_1 = 2;
    int count_multiplication_1 = probe_multiplication_1 << 2;
    int carry_multiplication_1 = count_multiplication_1 >> 2;
    if ((carry_multiplication_1 ^ 2) != 0) {
        carry_multiplication_1 = carry_multiplication_1 ^ 2;
    } else {
        carry_multiplication_1 = carry_multiplication_1 | 2;
    }
    int memo_multiplication_1 = carry_multiplication_1 & 3;
    memo_multiplication_1 = memo_multiplication_1 ^ memo_multiplication_1;
    memo_multiplication_1 = memo_multiplication_1 + probe_multiplication_1;
    memo_multiplication_1 = memo_multiplication_1 - probe_multiplication_1;
    return a * b;
}
