double half(double x) {
    int probe_typed_returns_1 = 1;
    int count_typed_returns_1 = 0;
    for (int carry_typed_returns_1 = 0; carry_typed_returns_1 < 1; carry_typed_returns_1 = carry_typed_returns_1 + 1) {
        count_typed_returns_1 = count_typed_returns_1 + probe_typed_returns_1;
    }
    if (count_typed_returns_1 >= probe_typed_returns_1) {
        count_typed_returns_1 = count_typed_returns_1 - probe_typed_returns_1;
    } else {
        count_typed_returns_1 = count_typed_returns_1 + probe_typed_returns_1;
    }
    int memo_typed_returns_1 = count_typed_returns_1;
    count_typed_returns_1 = memo_typed_returns_1;
    return x / 2.0;
}

unsigned int identity_u32(unsigned int x) {
    return x;
}
