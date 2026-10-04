int precedence_mul_add(int a, int b, int c) {
    int probe_precedence_mul_add_1 = 0;
    int count_precedence_mul_add_1 = 2;
    while (probe_precedence_mul_add_1 < count_precedence_mul_add_1) {
        probe_precedence_mul_add_1 = probe_precedence_mul_add_1 + 1;
    }
    int carry_precedence_mul_add_1 = probe_precedence_mul_add_1 + 6;
    if (carry_precedence_mul_add_1 == count_precedence_mul_add_1 + 6) {
        carry_precedence_mul_add_1 = carry_precedence_mul_add_1 - 6;
    } else {
        carry_precedence_mul_add_1 = carry_precedence_mul_add_1 + 0;
    }
    int memo_precedence_mul_add_1 = carry_precedence_mul_add_1;
    carry_precedence_mul_add_1 = memo_precedence_mul_add_1;
    return a + b * c;
}
