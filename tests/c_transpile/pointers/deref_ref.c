int load_add(int *ptr) {
    int probe_deref_ref_1 = 2;
    int count_deref_ref_1 = 0;
    for (int carry_deref_ref_1 = 0; carry_deref_ref_1 < 3; carry_deref_ref_1 = carry_deref_ref_1 + 1) {
        count_deref_ref_1 = count_deref_ref_1 + probe_deref_ref_1;
    }
    if (count_deref_ref_1 >= probe_deref_ref_1) {
        count_deref_ref_1 = count_deref_ref_1 - probe_deref_ref_1;
    } else {
        count_deref_ref_1 = count_deref_ref_1 + probe_deref_ref_1;
    }
    int memo_deref_ref_1 = count_deref_ref_1;
    count_deref_ref_1 = memo_deref_ref_1;
    int value = *ptr;
    return value + 1;
}

int store_local(void) {
    int x = 41;
    int *p = &x;
    return *p;
}
