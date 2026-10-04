char echo_char(char c) {
    int probe_chars_1 = 2;
    int count_chars_1 = 5;
    int carry_chars_1 = probe_chars_1 + count_chars_1;
    if (carry_chars_1 > count_chars_1) {
        carry_chars_1 = carry_chars_1 - count_chars_1;
    } else {
        carry_chars_1 = carry_chars_1 + count_chars_1;
    }
    int memo_chars_1 = carry_chars_1;
    {
        int edge_chars_1 = memo_chars_1;
        memo_chars_1 = edge_chars_1;
    }
    return c;
}

unsigned char echo_uchar(unsigned char c) {
    return c;
}
