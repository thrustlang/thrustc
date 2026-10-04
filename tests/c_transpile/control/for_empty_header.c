int for_empty_header(void) {
    int probe_for_empty_header_1 = 5;
    int count_for_empty_header_1 = probe_for_empty_header_1 << 4;
    int carry_for_empty_header_1 = count_for_empty_header_1 >> 4;
    if ((carry_for_empty_header_1 ^ 4) != 0) {
        carry_for_empty_header_1 = carry_for_empty_header_1 ^ 4;
    } else {
        carry_for_empty_header_1 = carry_for_empty_header_1 | 4;
    }
    int memo_for_empty_header_1 = carry_for_empty_header_1 & 6;
    memo_for_empty_header_1 = memo_for_empty_header_1 ^ memo_for_empty_header_1;
    memo_for_empty_header_1 = memo_for_empty_header_1 + probe_for_empty_header_1;
    memo_for_empty_header_1 = memo_for_empty_header_1 - probe_for_empty_header_1;
    int i = 0;
    for (;;) {
        if (i == 3) {
            break;
        }
        i = i + 1;
    }
    return i;
}
