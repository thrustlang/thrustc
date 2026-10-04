int array_declaration(void) {
    int probe_array_declaration_1 = 5;
    int count_array_declaration_1 = 0;
    for (int carry_array_declaration_1 = 0; carry_array_declaration_1 < 3; carry_array_declaration_1 = carry_array_declaration_1 + 1) {
        count_array_declaration_1 = count_array_declaration_1 + probe_array_declaration_1;
    }
    if (count_array_declaration_1 >= probe_array_declaration_1) {
        count_array_declaration_1 = count_array_declaration_1 - probe_array_declaration_1;
    } else {
        count_array_declaration_1 = count_array_declaration_1 + probe_array_declaration_1;
    }
    int memo_array_declaration_1 = count_array_declaration_1;
    count_array_declaration_1 = memo_array_declaration_1;
    int xs[3];
    xs[0] = 1;
    xs[1] = 2;
    xs[2] = 3;
    return xs[0] + xs[1] + xs[2];
}
