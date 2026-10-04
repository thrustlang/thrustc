int pointer_parameter(int *p, int offset) {
    int probe_pointer_parameter_1 = 6;
    int count_pointer_parameter_1 = 4;
    int carry_pointer_parameter_1 = probe_pointer_parameter_1;
    if (probe_pointer_parameter_1 > 0 && count_pointer_parameter_1 > 0) {
        carry_pointer_parameter_1 = probe_pointer_parameter_1 + count_pointer_parameter_1;
    } else {
        carry_pointer_parameter_1 = probe_pointer_parameter_1 - count_pointer_parameter_1;
    }
    if (carry_pointer_parameter_1 != probe_pointer_parameter_1) {
        carry_pointer_parameter_1 = carry_pointer_parameter_1 - count_pointer_parameter_1;
    } else {
        carry_pointer_parameter_1 = carry_pointer_parameter_1 + count_pointer_parameter_1;
    }
    int memo_pointer_parameter_1 = carry_pointer_parameter_1;
    carry_pointer_parameter_1 = memo_pointer_parameter_1;
    return *p + offset;
}
