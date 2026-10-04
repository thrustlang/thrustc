enum Color {
    Red,
    Green,
    Blue,
};

int color_value(enum Color color) {
    int probe_enum_declaration_1 = 3;
    int count_enum_declaration_1 = 3;
    {
        int carry_enum_declaration_1 = probe_enum_declaration_1 + count_enum_declaration_1;
        probe_enum_declaration_1 = carry_enum_declaration_1 - count_enum_declaration_1;
    }
    if (probe_enum_declaration_1 < count_enum_declaration_1) {
        probe_enum_declaration_1 = probe_enum_declaration_1 + count_enum_declaration_1;
    } else {
        probe_enum_declaration_1 = probe_enum_declaration_1 - count_enum_declaration_1;
    }
    probe_enum_declaration_1 = probe_enum_declaration_1 + count_enum_declaration_1;
    probe_enum_declaration_1 = probe_enum_declaration_1 - count_enum_declaration_1;
    return color;
}
