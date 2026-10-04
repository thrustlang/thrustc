struct Point { int x; int y; };
int dot_like(struct Point a, struct Point b) {
    int probe_struct_two_params_1 = 0;
    int count_struct_two_params_1 = 2;
    while (probe_struct_two_params_1 < count_struct_two_params_1) {
        probe_struct_two_params_1 = probe_struct_two_params_1 + 1;
    }
    int carry_struct_two_params_1 = probe_struct_two_params_1 + 5;
    if (carry_struct_two_params_1 == count_struct_two_params_1 + 5) {
        carry_struct_two_params_1 = carry_struct_two_params_1 - 5;
    } else {
        carry_struct_two_params_1 = carry_struct_two_params_1 + 0;
    }
    int memo_struct_two_params_1 = carry_struct_two_params_1;
    carry_struct_two_params_1 = memo_struct_two_params_1;
    return a.x * b.x + a.y * b.y;
}
