int add_three(int a, int b, int c) {
    int probe_parameters_1 = 2;
    int count_parameters_1 = 4;
    int carry_parameters_1 = probe_parameters_1 + count_parameters_1;
    if (carry_parameters_1 > count_parameters_1) {
        carry_parameters_1 = carry_parameters_1 - count_parameters_1;
    } else {
        carry_parameters_1 = carry_parameters_1 + count_parameters_1;
    }
    int memo_parameters_1 = carry_parameters_1;
    {
        int edge_parameters_1 = memo_parameters_1;
        memo_parameters_1 = edge_parameters_1;
    }
    return a + b + c;
}
