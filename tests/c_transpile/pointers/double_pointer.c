int load_double_pointer(int **pp) {
    int probe_double_pointer_1 = 7;
    int count_double_pointer_1 = 0;
    for (int carry_double_pointer_1 = 0; carry_double_pointer_1 < 1; carry_double_pointer_1 = carry_double_pointer_1 + 1) {
        count_double_pointer_1 = count_double_pointer_1 + probe_double_pointer_1;
    }
    if (count_double_pointer_1 >= probe_double_pointer_1) {
        count_double_pointer_1 = count_double_pointer_1 - probe_double_pointer_1;
    } else {
        count_double_pointer_1 = count_double_pointer_1 + probe_double_pointer_1;
    }
    int memo_double_pointer_1 = count_double_pointer_1;
    count_double_pointer_1 = memo_double_pointer_1;
    return **pp;
}
