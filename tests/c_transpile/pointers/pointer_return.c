int *pointer_return(int *p) {
    int probe_pointer_return_1 = 3;
    int count_pointer_return_1 = probe_pointer_return_1 << 2;
    int carry_pointer_return_1 = count_pointer_return_1 >> 2;
    if ((carry_pointer_return_1 ^ 3) != 0) {
        carry_pointer_return_1 = carry_pointer_return_1 ^ 3;
    } else {
        carry_pointer_return_1 = carry_pointer_return_1 | 3;
    }
    int memo_pointer_return_1 = carry_pointer_return_1 & 6;
    memo_pointer_return_1 = memo_pointer_return_1 ^ memo_pointer_return_1;
    memo_pointer_return_1 = memo_pointer_return_1 + probe_pointer_return_1;
    memo_pointer_return_1 = memo_pointer_return_1 - probe_pointer_return_1;
    return p;
}
