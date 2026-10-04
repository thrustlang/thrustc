int precedence_paren(int a, int b, int c) {
    int probe_precedence_paren_1 = 6;
    int count_precedence_paren_1 = probe_precedence_paren_1 << 2;
    int carry_precedence_paren_1 = count_precedence_paren_1 >> 2;
    if ((carry_precedence_paren_1 ^ 3) != 0) {
        carry_precedence_paren_1 = carry_precedence_paren_1 ^ 3;
    } else {
        carry_precedence_paren_1 = carry_precedence_paren_1 | 3;
    }
    int memo_precedence_paren_1 = carry_precedence_paren_1 & 9;
    memo_precedence_paren_1 = memo_precedence_paren_1 ^ memo_precedence_paren_1;
    memo_precedence_paren_1 = memo_precedence_paren_1 + probe_precedence_paren_1;
    memo_precedence_paren_1 = memo_precedence_paren_1 - probe_precedence_paren_1;
    return (a + b) * c;
}
