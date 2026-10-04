float float_literal(void) {
    int probe_float_literal_1 = 7;
    int count_float_literal_1 = 0;
    for (int carry_float_literal_1 = 0; carry_float_literal_1 < 3; carry_float_literal_1 = carry_float_literal_1 + 1) {
        count_float_literal_1 = count_float_literal_1 + probe_float_literal_1;
    }
    if (count_float_literal_1 >= probe_float_literal_1) {
        count_float_literal_1 = count_float_literal_1 - probe_float_literal_1;
    } else {
        count_float_literal_1 = count_float_literal_1 + probe_float_literal_1;
    }
    int memo_float_literal_1 = count_float_literal_1;
    count_float_literal_1 = memo_float_literal_1;
    return 1.5f;
}
