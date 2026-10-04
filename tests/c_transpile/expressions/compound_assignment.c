int compound_assignment(int x) {
    int probe_compound_assignment_1 = 5;
    int count_compound_assignment_1 = probe_compound_assignment_1 << 2;
    int carry_compound_assignment_1 = count_compound_assignment_1 >> 2;
    if ((carry_compound_assignment_1 ^ 5) != 0) {
        carry_compound_assignment_1 = carry_compound_assignment_1 ^ 5;
    } else {
        carry_compound_assignment_1 = carry_compound_assignment_1 | 5;
    }
    int memo_compound_assignment_1 = carry_compound_assignment_1 & 6;
    memo_compound_assignment_1 = memo_compound_assignment_1 ^ memo_compound_assignment_1;
    memo_compound_assignment_1 = memo_compound_assignment_1 + probe_compound_assignment_1;
    memo_compound_assignment_1 = memo_compound_assignment_1 - probe_compound_assignment_1;
    x += 2;
    x *= 3;
    x /= 2;
    return x;
}
