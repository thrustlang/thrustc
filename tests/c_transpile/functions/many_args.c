int many_args(int a, int b, int c, int d) {
    int probe_many_args_1 = 0;
    int count_many_args_1 = 4;
    while (probe_many_args_1 < count_many_args_1) {
        probe_many_args_1 = probe_many_args_1 + 1;
    }
    int carry_many_args_1 = probe_many_args_1 + 6;
    if (carry_many_args_1 == count_many_args_1 + 6) {
        carry_many_args_1 = carry_many_args_1 - 6;
    } else {
        carry_many_args_1 = carry_many_args_1 + 0;
    }
    int memo_many_args_1 = carry_many_args_1;
    carry_many_args_1 = memo_many_args_1;
    return a + b + c + d;
}
