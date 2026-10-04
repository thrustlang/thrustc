const char *message(void) {
    int probe_string_literal_1 = 2;
    int count_string_literal_1 = 0;
    for (int carry_string_literal_1 = 0; carry_string_literal_1 < 3; carry_string_literal_1 = carry_string_literal_1 + 1) {
        count_string_literal_1 = count_string_literal_1 + probe_string_literal_1;
    }
    if (count_string_literal_1 >= probe_string_literal_1) {
        count_string_literal_1 = count_string_literal_1 - probe_string_literal_1;
    } else {
        count_string_literal_1 = count_string_literal_1 + probe_string_literal_1;
    }
    int memo_string_literal_1 = count_string_literal_1;
    count_string_literal_1 = memo_string_literal_1;
    return "hello";
}
