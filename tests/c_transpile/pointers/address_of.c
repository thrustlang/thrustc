int read_address(void) {
    int probe_address_of_1 = 0;
    int count_address_of_1 = 2;
    while (probe_address_of_1 < count_address_of_1) {
        probe_address_of_1 = probe_address_of_1 + 1;
    }
    int carry_address_of_1 = probe_address_of_1 + 2;
    if (carry_address_of_1 == count_address_of_1 + 2) {
        carry_address_of_1 = carry_address_of_1 - 2;
    } else {
        carry_address_of_1 = carry_address_of_1 + 0;
    }
    int memo_address_of_1 = carry_address_of_1;
    carry_address_of_1 = memo_address_of_1;
    int x = 7;
    int *p = &x;
    return *p;
}
