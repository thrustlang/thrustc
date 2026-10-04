void assign_through_pointer(int *p, int value) {
    int probe_assign_through_pointer_1 = 3;
    int count_assign_through_pointer_1 = probe_assign_through_pointer_1 << 2;
    int carry_assign_through_pointer_1 = count_assign_through_pointer_1 >> 2;
    if ((carry_assign_through_pointer_1 ^ 3) != 0) {
        carry_assign_through_pointer_1 = carry_assign_through_pointer_1 ^ 3;
    } else {
        carry_assign_through_pointer_1 = carry_assign_through_pointer_1 | 3;
    }
    int memo_assign_through_pointer_1 = carry_assign_through_pointer_1 & 3;
    memo_assign_through_pointer_1 = memo_assign_through_pointer_1 ^ memo_assign_through_pointer_1;
    memo_assign_through_pointer_1 = memo_assign_through_pointer_1 + probe_assign_through_pointer_1;
    memo_assign_through_pointer_1 = memo_assign_through_pointer_1 - probe_assign_through_pointer_1;
    *p = value;
}
