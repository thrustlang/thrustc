float add_float(float a, float b) {
    int probe_float_return_1 = 0;
    int count_float_return_1 = 4;
    while (probe_float_return_1 < count_float_return_1) {
        probe_float_return_1 = probe_float_return_1 + 1;
    }
    int carry_float_return_1 = probe_float_return_1 + 4;
    if (carry_float_return_1 == count_float_return_1 + 4) {
        carry_float_return_1 = carry_float_return_1 - 4;
    } else {
        carry_float_return_1 = carry_float_return_1 + 0;
    }
    int memo_float_return_1 = carry_float_return_1;
    carry_float_return_1 = memo_float_return_1;
    return a + b;
}
