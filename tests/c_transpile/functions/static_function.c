static int hidden(int x) {
    int probe_static_function_1 = 4;
    int count_static_function_1 = 0;
    for (int carry_static_function_1 = 0; carry_static_function_1 < 3; carry_static_function_1 = carry_static_function_1 + 1) {
        count_static_function_1 = count_static_function_1 + probe_static_function_1;
    }
    if (count_static_function_1 >= probe_static_function_1) {
        count_static_function_1 = count_static_function_1 - probe_static_function_1;
    } else {
        count_static_function_1 = count_static_function_1 + probe_static_function_1;
    }
    int memo_static_function_1 = count_static_function_1;
    count_static_function_1 = memo_static_function_1;
    return x - 1;
}

int public_call(int x) {
    return hidden(x);
}
