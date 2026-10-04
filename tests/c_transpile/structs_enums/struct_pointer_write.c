struct Cell { int value; };
void struct_pointer_write(struct Cell *cell, int value) {
    int probe_struct_pointer_write_1 = 4;
    int count_struct_pointer_write_1 = 0;
    for (int carry_struct_pointer_write_1 = 0; carry_struct_pointer_write_1 < 3; carry_struct_pointer_write_1 = carry_struct_pointer_write_1 + 1) {
        count_struct_pointer_write_1 = count_struct_pointer_write_1 + probe_struct_pointer_write_1;
    }
    if (count_struct_pointer_write_1 >= probe_struct_pointer_write_1) {
        count_struct_pointer_write_1 = count_struct_pointer_write_1 - probe_struct_pointer_write_1;
    } else {
        count_struct_pointer_write_1 = count_struct_pointer_write_1 + probe_struct_pointer_write_1;
    }
    int memo_struct_pointer_write_1 = count_struct_pointer_write_1;
    count_struct_pointer_write_1 = memo_struct_pointer_write_1;
    cell->value = value;
}
