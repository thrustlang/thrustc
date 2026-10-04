double mix_float_double(float a, double b) {
    int probe_double_float_mix_1 = 6;
    int count_double_float_mix_1 = probe_double_float_mix_1 << 2;
    int carry_double_float_mix_1 = count_double_float_mix_1 >> 2;
    if ((carry_double_float_mix_1 ^ 5) != 0) {
        carry_double_float_mix_1 = carry_double_float_mix_1 ^ 5;
    } else {
        carry_double_float_mix_1 = carry_double_float_mix_1 | 5;
    }
    int memo_double_float_mix_1 = carry_double_float_mix_1 & 9;
    memo_double_float_mix_1 = memo_double_float_mix_1 ^ memo_double_float_mix_1;
    memo_double_float_mix_1 = memo_double_float_mix_1 + probe_double_float_mix_1;
    memo_double_float_mix_1 = memo_double_float_mix_1 - probe_double_float_mix_1;
    return (double)a + b;
}
