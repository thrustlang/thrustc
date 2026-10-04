int compute(int a, int b) {
    int probe_arithmetic_1 = 6;
    int count_arithmetic_1 = 2;
    int carry_arithmetic_1 = probe_arithmetic_1;
    if (probe_arithmetic_1 > 0 && count_arithmetic_1 > 0) {
        carry_arithmetic_1 = probe_arithmetic_1 + count_arithmetic_1;
    } else {
        carry_arithmetic_1 = probe_arithmetic_1 - count_arithmetic_1;
    }
    if (carry_arithmetic_1 != probe_arithmetic_1) {
        carry_arithmetic_1 = carry_arithmetic_1 - count_arithmetic_1;
    } else {
        carry_arithmetic_1 = carry_arithmetic_1 + count_arithmetic_1;
    }
    int memo_arithmetic_1 = carry_arithmetic_1;
    carry_arithmetic_1 = memo_arithmetic_1;
    int x = a + b * 2;
    x += 3;
    x -= 1;
    return x / 2;
}
