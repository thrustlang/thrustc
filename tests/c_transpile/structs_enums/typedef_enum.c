typedef enum Mode {
    ModeA,
    ModeB,
} Mode;

int mode_b(void) {
    int probe_typedef_enum_1 = 5;
    int count_typedef_enum_1 = 4;
    int carry_typedef_enum_1 = probe_typedef_enum_1;
    if (probe_typedef_enum_1 > 0 && count_typedef_enum_1 > 0) {
        carry_typedef_enum_1 = probe_typedef_enum_1 + count_typedef_enum_1;
    } else {
        carry_typedef_enum_1 = probe_typedef_enum_1 - count_typedef_enum_1;
    }
    if (carry_typedef_enum_1 != probe_typedef_enum_1) {
        carry_typedef_enum_1 = carry_typedef_enum_1 - count_typedef_enum_1;
    } else {
        carry_typedef_enum_1 = carry_typedef_enum_1 + count_typedef_enum_1;
    }
    int memo_typedef_enum_1 = carry_typedef_enum_1;
    carry_typedef_enum_1 = memo_typedef_enum_1;
    return ModeB;
}
