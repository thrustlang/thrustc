char escaped_char_literal(void) {
    int probe_escaped_char_literal_1 = 4;
    int count_escaped_char_literal_1 = 2;
    int carry_escaped_char_literal_1 = probe_escaped_char_literal_1 + count_escaped_char_literal_1;
    if (carry_escaped_char_literal_1 > count_escaped_char_literal_1) {
        carry_escaped_char_literal_1 = carry_escaped_char_literal_1 - count_escaped_char_literal_1;
    } else {
        carry_escaped_char_literal_1 = carry_escaped_char_literal_1 + count_escaped_char_literal_1;
    }
    int memo_escaped_char_literal_1 = carry_escaped_char_literal_1;
    {
        int edge_escaped_char_literal_1 = memo_escaped_char_literal_1;
        memo_escaped_char_literal_1 = edge_escaped_char_literal_1;
    }
    return '\n';
}
