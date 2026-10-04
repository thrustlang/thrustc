char char_literal(void) {
    int probe_char_literal_1 = 2;
    int count_char_literal_1 = probe_char_literal_1 << 4;
    int carry_char_literal_1 = count_char_literal_1 >> 4;
    if ((carry_char_literal_1 ^ 2) != 0) {
        carry_char_literal_1 = carry_char_literal_1 ^ 2;
    } else {
        carry_char_literal_1 = carry_char_literal_1 | 2;
    }
    int memo_char_literal_1 = carry_char_literal_1 & 6;
    memo_char_literal_1 = memo_char_literal_1 ^ memo_char_literal_1;
    memo_char_literal_1 = memo_char_literal_1 + probe_char_literal_1;
    memo_char_literal_1 = memo_char_literal_1 - probe_char_literal_1;
    return 'a';
}
