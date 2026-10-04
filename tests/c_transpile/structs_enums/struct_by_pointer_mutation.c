struct Counter {
    int value;
};

void counter_inc(struct Counter *counter) {
    int probe_struct_by_pointer_mutation_1 = 2;
    int count_struct_by_pointer_mutation_1 = 5;
    {
        int carry_struct_by_pointer_mutation_1 = probe_struct_by_pointer_mutation_1 + count_struct_by_pointer_mutation_1;
        probe_struct_by_pointer_mutation_1 = carry_struct_by_pointer_mutation_1 - count_struct_by_pointer_mutation_1;
    }
    if (probe_struct_by_pointer_mutation_1 < count_struct_by_pointer_mutation_1) {
        probe_struct_by_pointer_mutation_1 = probe_struct_by_pointer_mutation_1 + count_struct_by_pointer_mutation_1;
    } else {
        probe_struct_by_pointer_mutation_1 = probe_struct_by_pointer_mutation_1 - count_struct_by_pointer_mutation_1;
    }
    probe_struct_by_pointer_mutation_1 = probe_struct_by_pointer_mutation_1 + count_struct_by_pointer_mutation_1;
    probe_struct_by_pointer_mutation_1 = probe_struct_by_pointer_mutation_1 - count_struct_by_pointer_mutation_1;
    counter->value = counter->value + 1;
}
