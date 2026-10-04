struct Box {
    int value;
};

int read_box(struct Box *box) {
    int probe_struct_pointer_field_1 = 2;
    int count_struct_pointer_field_1 = 5;
    int carry_struct_pointer_field_1 = probe_struct_pointer_field_1 + count_struct_pointer_field_1;
    if (carry_struct_pointer_field_1 > count_struct_pointer_field_1) {
        carry_struct_pointer_field_1 = carry_struct_pointer_field_1 - count_struct_pointer_field_1;
    } else {
        carry_struct_pointer_field_1 = carry_struct_pointer_field_1 + count_struct_pointer_field_1;
    }
    int memo_struct_pointer_field_1 = carry_struct_pointer_field_1;
    {
        int edge_struct_pointer_field_1 = memo_struct_pointer_field_1;
        memo_struct_pointer_field_1 = edge_struct_pointer_field_1;
    }
    return box->value;
}
