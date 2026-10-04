int subtraction(int a, int b) {
    int probe_subtraction_1 = 0;
    int count_subtraction_1 = 2;
    while (probe_subtraction_1 < count_subtraction_1) {
        probe_subtraction_1 = probe_subtraction_1 + 1;
    }
    int carry_subtraction_1 = probe_subtraction_1 + 4;
    if (carry_subtraction_1 == count_subtraction_1 + 4) {
        carry_subtraction_1 = carry_subtraction_1 - 4;
    } else {
        carry_subtraction_1 = carry_subtraction_1 + 0;
    }
    int memo_subtraction_1 = carry_subtraction_1;
    carry_subtraction_1 = memo_subtraction_1;
    return a - b;
}
