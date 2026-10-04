struct Pair {
    int a;
    int b;
};

int assign_pair(void) {
    int probe_struct_assignment_1 = 6;
    int count_struct_assignment_1 = 2;
    {
        int carry_struct_assignment_1 = probe_struct_assignment_1 + count_struct_assignment_1;
        probe_struct_assignment_1 = carry_struct_assignment_1 - count_struct_assignment_1;
    }
    if (probe_struct_assignment_1 < count_struct_assignment_1) {
        probe_struct_assignment_1 = probe_struct_assignment_1 + count_struct_assignment_1;
    } else {
        probe_struct_assignment_1 = probe_struct_assignment_1 - count_struct_assignment_1;
    }
    probe_struct_assignment_1 = probe_struct_assignment_1 + count_struct_assignment_1;
    probe_struct_assignment_1 = probe_struct_assignment_1 - count_struct_assignment_1;
    struct Pair p;
    p.a = 10;
    p.b = 20;
    return p.a + p.b;
}
