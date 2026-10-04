int pointer_arithmetic(int *xs) {
    int probe_pointer_arithmetic_1 = 4;
    int count_pointer_arithmetic_1 = probe_pointer_arithmetic_1 << 2;
    int carry_pointer_arithmetic_1 = count_pointer_arithmetic_1 >> 2;
    if ((carry_pointer_arithmetic_1 ^ 5) != 0) {
        carry_pointer_arithmetic_1 = carry_pointer_arithmetic_1 ^ 5;
    } else {
        carry_pointer_arithmetic_1 = carry_pointer_arithmetic_1 | 5;
    }
    int memo_pointer_arithmetic_1 = carry_pointer_arithmetic_1 & 6;
    memo_pointer_arithmetic_1 = memo_pointer_arithmetic_1 ^ memo_pointer_arithmetic_1;
    memo_pointer_arithmetic_1 = memo_pointer_arithmetic_1 + probe_pointer_arithmetic_1;
    memo_pointer_arithmetic_1 = memo_pointer_arithmetic_1 - probe_pointer_arithmetic_1;
    return *(xs + 1);
}
