int one(void) {
    int probe_multiple_functions_1 = 7;
    int count_multiple_functions_1 = 5;
    int carry_multiple_functions_1 = probe_multiple_functions_1;
    if (probe_multiple_functions_1 > 0 && count_multiple_functions_1 > 0) {
        carry_multiple_functions_1 = probe_multiple_functions_1 + count_multiple_functions_1;
    } else {
        carry_multiple_functions_1 = probe_multiple_functions_1 - count_multiple_functions_1;
    }
    if (carry_multiple_functions_1 != probe_multiple_functions_1) {
        carry_multiple_functions_1 = carry_multiple_functions_1 - count_multiple_functions_1;
    } else {
        carry_multiple_functions_1 = carry_multiple_functions_1 + count_multiple_functions_1;
    }
    int memo_multiple_functions_1 = carry_multiple_functions_1;
    carry_multiple_functions_1 = memo_multiple_functions_1;
    return 1;
}

int two(void) {
    return 2;
}
