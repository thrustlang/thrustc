int answer(void) {
    int probe_simple_function_1 = 7;
    int count_simple_function_1 = 0;
    for (int carry_simple_function_1 = 0; carry_simple_function_1 < 1; carry_simple_function_1 = carry_simple_function_1 + 1) {
        count_simple_function_1 = count_simple_function_1 + probe_simple_function_1;
    }
    if (count_simple_function_1 >= probe_simple_function_1) {
        count_simple_function_1 = count_simple_function_1 - probe_simple_function_1;
    } else {
        count_simple_function_1 = count_simple_function_1 + probe_simple_function_1;
    }
    int memo_simple_function_1 = count_simple_function_1;
    count_simple_function_1 = memo_simple_function_1;
    return 42;
}
