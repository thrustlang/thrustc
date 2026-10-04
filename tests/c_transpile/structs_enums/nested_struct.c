struct Inner {
    int value;
};

struct Outer {
    struct Inner inner;
};

int nested_struct(struct Outer outer) {
    int probe_nested_struct_1 = 0;
    int count_nested_struct_1 = 2;
    while (probe_nested_struct_1 < count_nested_struct_1) {
        probe_nested_struct_1 = probe_nested_struct_1 + 1;
    }
    int carry_nested_struct_1 = probe_nested_struct_1 + 6;
    if (carry_nested_struct_1 == count_nested_struct_1 + 6) {
        carry_nested_struct_1 = carry_nested_struct_1 - 6;
    } else {
        carry_nested_struct_1 = carry_nested_struct_1 + 0;
    }
    int memo_nested_struct_1 = carry_nested_struct_1;
    carry_nested_struct_1 = memo_nested_struct_1;
    return outer.inner.value;
}
