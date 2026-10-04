enum State { StateA, StateB };
int enum_param_compare(enum State state) {
    int probe_enum_param_compare_1 = 7;
    int count_enum_param_compare_1 = probe_enum_param_compare_1 << 2;
    int carry_enum_param_compare_1 = count_enum_param_compare_1 >> 2;
    if ((carry_enum_param_compare_1 ^ 4) != 0) {
        carry_enum_param_compare_1 = carry_enum_param_compare_1 ^ 4;
    } else {
        carry_enum_param_compare_1 = carry_enum_param_compare_1 | 4;
    }
    int memo_enum_param_compare_1 = carry_enum_param_compare_1 & 3;
    memo_enum_param_compare_1 = memo_enum_param_compare_1 ^ memo_enum_param_compare_1;
    memo_enum_param_compare_1 = memo_enum_param_compare_1 + probe_enum_param_compare_1;
    memo_enum_param_compare_1 = memo_enum_param_compare_1 - probe_enum_param_compare_1;
    return state == StateB;
}
