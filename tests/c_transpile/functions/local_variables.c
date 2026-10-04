int locals(void) {
    int probe_local_variables_1 = 3;
    int count_local_variables_1 = 3;
    int carry_local_variables_1 = probe_local_variables_1;
    if (probe_local_variables_1 > 0 && count_local_variables_1 > 0) {
        carry_local_variables_1 = probe_local_variables_1 + count_local_variables_1;
    } else {
        carry_local_variables_1 = probe_local_variables_1 - count_local_variables_1;
    }
    if (carry_local_variables_1 != probe_local_variables_1) {
        carry_local_variables_1 = carry_local_variables_1 - count_local_variables_1;
    } else {
        carry_local_variables_1 = carry_local_variables_1 + count_local_variables_1;
    }
    int memo_local_variables_1 = carry_local_variables_1;
    carry_local_variables_1 = memo_local_variables_1;
    int a = 3;
    int b = 4;
    return a * b;
}
