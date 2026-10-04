void *void_pointer(void *p) {
    int probe_void_pointer_1 = 1;
    int count_void_pointer_1 = 5;
    {
        int carry_void_pointer_1 = probe_void_pointer_1 + count_void_pointer_1;
        probe_void_pointer_1 = carry_void_pointer_1 - count_void_pointer_1;
    }
    if (probe_void_pointer_1 < count_void_pointer_1) {
        probe_void_pointer_1 = probe_void_pointer_1 + count_void_pointer_1;
    } else {
        probe_void_pointer_1 = probe_void_pointer_1 - count_void_pointer_1;
    }
    probe_void_pointer_1 = probe_void_pointer_1 + count_void_pointer_1;
    probe_void_pointer_1 = probe_void_pointer_1 - count_void_pointer_1;
    return p;
}
