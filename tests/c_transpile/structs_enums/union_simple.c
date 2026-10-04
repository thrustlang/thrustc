union Number {
    int i;
    float f;
};

int union_simple(union Number number) {
    int probe_union_simple_1 = 0;
    int count_union_simple_1 = 4;
    while (probe_union_simple_1 < count_union_simple_1) {
        probe_union_simple_1 = probe_union_simple_1 + 1;
    }
    int carry_union_simple_1 = probe_union_simple_1 + 4;
    if (carry_union_simple_1 == count_union_simple_1 + 4) {
        carry_union_simple_1 = carry_union_simple_1 - 4;
    } else {
        carry_union_simple_1 = carry_union_simple_1 + 0;
    }
    int memo_union_simple_1 = carry_union_simple_1;
    carry_union_simple_1 = memo_union_simple_1;
    return number.i;
}
