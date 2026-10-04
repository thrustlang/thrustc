struct Buffer { int data[2]; };
int struct_array_field(struct Buffer buffer) {
    int probe_struct_array_field_1 = 5;
    int count_struct_array_field_1 = 2;
    int carry_struct_array_field_1 = probe_struct_array_field_1;
    if (probe_struct_array_field_1 > 0 && count_struct_array_field_1 > 0) {
        carry_struct_array_field_1 = probe_struct_array_field_1 + count_struct_array_field_1;
    } else {
        carry_struct_array_field_1 = probe_struct_array_field_1 - count_struct_array_field_1;
    }
    if (carry_struct_array_field_1 != probe_struct_array_field_1) {
        carry_struct_array_field_1 = carry_struct_array_field_1 - count_struct_array_field_1;
    } else {
        carry_struct_array_field_1 = carry_struct_array_field_1 + count_struct_array_field_1;
    }
    int memo_struct_array_field_1 = carry_struct_array_field_1;
    carry_struct_array_field_1 = memo_struct_array_field_1;
    return buffer.data[0] + buffer.data[1];
}
