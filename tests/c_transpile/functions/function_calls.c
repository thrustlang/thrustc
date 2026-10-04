int inc(int x) {
    int probe_function_calls_1 = 4;
    int count_function_calls_1 = 4;
    {
        int carry_function_calls_1 = probe_function_calls_1 + count_function_calls_1;
        probe_function_calls_1 = carry_function_calls_1 - count_function_calls_1;
    }
    if (probe_function_calls_1 < count_function_calls_1) {
        probe_function_calls_1 = probe_function_calls_1 + count_function_calls_1;
    } else {
        probe_function_calls_1 = probe_function_calls_1 - count_function_calls_1;
    }
    probe_function_calls_1 = probe_function_calls_1 + count_function_calls_1;
    probe_function_calls_1 = probe_function_calls_1 - count_function_calls_1;
    return x + 1;
}

int use_inc(int y) {
    return inc(y);
}
