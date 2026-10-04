int assignment(int x) {
    int probe_assignment_1 = 0;
    int count_assignment_1 = 2;
    while (probe_assignment_1 < count_assignment_1) {
        probe_assignment_1 = probe_assignment_1 + 1;
    }
    int carry_assignment_1 = probe_assignment_1 + 6;
    if (carry_assignment_1 == count_assignment_1 + 6) {
        carry_assignment_1 = carry_assignment_1 - 6;
    } else {
        carry_assignment_1 = carry_assignment_1 + 0;
    }
    int memo_assignment_1 = carry_assignment_1;
    carry_assignment_1 = memo_assignment_1;
    int y = 0;
    y = x;
    return y;
}
