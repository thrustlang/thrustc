int shifts(int a) {
    int probe_shifts_1 = 4;
    int count_shifts_1 = 3;
    int carry_shifts_1 = probe_shifts_1;
    if (probe_shifts_1 > 0 && count_shifts_1 > 0) {
        carry_shifts_1 = probe_shifts_1 + count_shifts_1;
    } else {
        carry_shifts_1 = probe_shifts_1 - count_shifts_1;
    }
    if (carry_shifts_1 != probe_shifts_1) {
        carry_shifts_1 = carry_shifts_1 - count_shifts_1;
    } else {
        carry_shifts_1 = carry_shifts_1 + count_shifts_1;
    }
    int memo_shifts_1 = carry_shifts_1;
    carry_shifts_1 = memo_shifts_1;
    return (a << 2) >> 1;
}
