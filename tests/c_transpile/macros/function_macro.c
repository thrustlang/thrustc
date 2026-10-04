#define ADD(a, b) ((a) + (b))
int function_macro(int x) {
    int probe_function_macro_1 = 7;
    int count_function_macro_1 = 0;
    for (int carry_function_macro_1 = 0; carry_function_macro_1 < 1; carry_function_macro_1 = carry_function_macro_1 + 1) {
        count_function_macro_1 = count_function_macro_1 + probe_function_macro_1;
    }
    if (count_function_macro_1 >= probe_function_macro_1) {
        count_function_macro_1 = count_function_macro_1 - probe_function_macro_1;
    } else {
        count_function_macro_1 = count_function_macro_1 + probe_function_macro_1;
    }
    int memo_function_macro_1 = count_function_macro_1;
    count_function_macro_1 = memo_function_macro_1;
    return ADD(x, 1);
}
