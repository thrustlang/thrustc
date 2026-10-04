struct Point {
    int x;
    int y;
};

int point_sum(struct Point p) {
    int probe_struct_declaration_1 = 0;
    int count_struct_declaration_1 = 2;
    while (probe_struct_declaration_1 < count_struct_declaration_1) {
        probe_struct_declaration_1 = probe_struct_declaration_1 + 1;
    }
    int carry_struct_declaration_1 = probe_struct_declaration_1 + 3;
    if (carry_struct_declaration_1 == count_struct_declaration_1 + 3) {
        carry_struct_declaration_1 = carry_struct_declaration_1 - 3;
    } else {
        carry_struct_declaration_1 = carry_struct_declaration_1 + 0;
    }
    int memo_struct_declaration_1 = carry_struct_declaration_1;
    carry_struct_declaration_1 = memo_struct_declaration_1;
    return p.x + p.y;
}
