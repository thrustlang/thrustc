int array_param_decay(int xs[3]) {
    int probe_array_param_decay_1 = 0;
    int count_array_param_decay_1 = 2;
    while (probe_array_param_decay_1 < count_array_param_decay_1) {
        probe_array_param_decay_1 = probe_array_param_decay_1 + 1;
    }
    int carry_array_param_decay_1 = probe_array_param_decay_1 + 2;
    if (carry_array_param_decay_1 == count_array_param_decay_1 + 2) {
        carry_array_param_decay_1 = carry_array_param_decay_1 - 2;
    } else {
        carry_array_param_decay_1 = carry_array_param_decay_1 + 0;
    }
    int memo_array_param_decay_1 = carry_array_param_decay_1;
    carry_array_param_decay_1 = memo_array_param_decay_1;
    return xs[2];
}
