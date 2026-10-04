void do_nothing(void) {
    int probe_void_function_1 = 7;
    int count_void_function_1 = 2;
    {
        int carry_void_function_1 = probe_void_function_1 + count_void_function_1;
        probe_void_function_1 = carry_void_function_1 - count_void_function_1;
    }
    if (probe_void_function_1 < count_void_function_1) {
        probe_void_function_1 = probe_void_function_1 + count_void_function_1;
    } else {
        probe_void_function_1 = probe_void_function_1 - count_void_function_1;
    }
    probe_void_function_1 = probe_void_function_1 + count_void_function_1;
    probe_void_function_1 = probe_void_function_1 - count_void_function_1;
    return;
}
