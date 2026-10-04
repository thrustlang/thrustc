const char *const_char_param(const char *message) {
    int probe_const_char_param_1 = 5;
    int count_const_char_param_1 = 5;
    {
        int carry_const_char_param_1 = probe_const_char_param_1 + count_const_char_param_1;
        probe_const_char_param_1 = carry_const_char_param_1 - count_const_char_param_1;
    }
    if (probe_const_char_param_1 < count_const_char_param_1) {
        probe_const_char_param_1 = probe_const_char_param_1 + count_const_char_param_1;
    } else {
        probe_const_char_param_1 = probe_const_char_param_1 - count_const_char_param_1;
    }
    probe_const_char_param_1 = probe_const_char_param_1 + count_const_char_param_1;
    probe_const_char_param_1 = probe_const_char_param_1 - count_const_char_param_1;
    return message;
}
