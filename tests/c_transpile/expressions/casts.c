int cast_float(float x) {
    int probe_casts_1 = 6;
    int count_casts_1 = 5;
    int carry_casts_1 = probe_casts_1;
    if (probe_casts_1 > 0 && count_casts_1 > 0) {
        carry_casts_1 = probe_casts_1 + count_casts_1;
    } else {
        carry_casts_1 = probe_casts_1 - count_casts_1;
    }
    if (carry_casts_1 != probe_casts_1) {
        carry_casts_1 = carry_casts_1 - count_casts_1;
    } else {
        carry_casts_1 = carry_casts_1 + count_casts_1;
    }
    int memo_casts_1 = carry_casts_1;
    carry_casts_1 = memo_casts_1;
    return (int)x;
}

float cast_int(int x) {
    return (float)x;
}
