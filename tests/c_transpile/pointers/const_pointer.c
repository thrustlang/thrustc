int const_pointer(const int *p) {
    int probe_const_pointer_1 = 0;
    int count_const_pointer_1 = 4;
    while (probe_const_pointer_1 < count_const_pointer_1) {
        probe_const_pointer_1 = probe_const_pointer_1 + 1;
    }
    int carry_const_pointer_1 = probe_const_pointer_1 + 6;
    if (carry_const_pointer_1 == count_const_pointer_1 + 6) {
        carry_const_pointer_1 = carry_const_pointer_1 - 6;
    } else {
        carry_const_pointer_1 = carry_const_pointer_1 + 0;
    }
    int memo_const_pointer_1 = carry_const_pointer_1;
    carry_const_pointer_1 = memo_const_pointer_1;
    return *p;
}
