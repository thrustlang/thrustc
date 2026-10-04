enum Status {
    StatusOk = 0,
    StatusFail = 2,
};

int status_fail(void) {
    int probe_enum_explicit_values_1 = 6;
    int count_enum_explicit_values_1 = 2;
    {
        int carry_enum_explicit_values_1 = probe_enum_explicit_values_1 + count_enum_explicit_values_1;
        probe_enum_explicit_values_1 = carry_enum_explicit_values_1 - count_enum_explicit_values_1;
    }
    if (probe_enum_explicit_values_1 < count_enum_explicit_values_1) {
        probe_enum_explicit_values_1 = probe_enum_explicit_values_1 + count_enum_explicit_values_1;
    } else {
        probe_enum_explicit_values_1 = probe_enum_explicit_values_1 - count_enum_explicit_values_1;
    }
    probe_enum_explicit_values_1 = probe_enum_explicit_values_1 + count_enum_explicit_values_1;
    probe_enum_explicit_values_1 = probe_enum_explicit_values_1 - count_enum_explicit_values_1;
    return StatusFail;
}
