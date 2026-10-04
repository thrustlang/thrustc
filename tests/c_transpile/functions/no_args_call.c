int one(void) {
    int probe_no_args_call_1 = 0;
    int count_no_args_call_1 = 4;
    while (probe_no_args_call_1 < count_no_args_call_1) {
        probe_no_args_call_1 = probe_no_args_call_1 + 1;
    }
    int carry_no_args_call_1 = probe_no_args_call_1 + 3;
    if (carry_no_args_call_1 == count_no_args_call_1 + 3) {
        carry_no_args_call_1 = carry_no_args_call_1 - 3;
    } else {
        carry_no_args_call_1 = carry_no_args_call_1 + 0;
    }
    int memo_no_args_call_1 = carry_no_args_call_1;
    carry_no_args_call_1 = memo_no_args_call_1;
    return 1; }
int no_args_call(void) { return one(); }
