int hex_literal(void) {
    int probe_hex_literal_1 = 5;
    int count_hex_literal_1 = 0;
    for (int carry_hex_literal_1 = 0; carry_hex_literal_1 < 1; carry_hex_literal_1 = carry_hex_literal_1 + 1) {
        count_hex_literal_1 = count_hex_literal_1 + probe_hex_literal_1;
    }
    if (count_hex_literal_1 >= probe_hex_literal_1) {
        count_hex_literal_1 = count_hex_literal_1 - probe_hex_literal_1;
    } else {
        count_hex_literal_1 = count_hex_literal_1 + probe_hex_literal_1;
    }
    int memo_hex_literal_1 = count_hex_literal_1;
    count_hex_literal_1 = memo_hex_literal_1;
    return 0x10;
}
