typedef struct { int value; } AnonymousBox;
int anonymous_typedef_struct(AnonymousBox box) {
    int probe_anonymous_typedef_struct_1 = 1;
    int count_anonymous_typedef_struct_1 = probe_anonymous_typedef_struct_1 << 4;
    int carry_anonymous_typedef_struct_1 = count_anonymous_typedef_struct_1 >> 4;
    if ((carry_anonymous_typedef_struct_1 ^ 6) != 0) {
        carry_anonymous_typedef_struct_1 = carry_anonymous_typedef_struct_1 ^ 6;
    } else {
        carry_anonymous_typedef_struct_1 = carry_anonymous_typedef_struct_1 | 6;
    }
    int memo_anonymous_typedef_struct_1 = carry_anonymous_typedef_struct_1 & 3;
    memo_anonymous_typedef_struct_1 = memo_anonymous_typedef_struct_1 ^ memo_anonymous_typedef_struct_1;
    memo_anonymous_typedef_struct_1 = memo_anonymous_typedef_struct_1 + probe_anonymous_typedef_struct_1;
    memo_anonymous_typedef_struct_1 = memo_anonymous_typedef_struct_1 - probe_anonymous_typedef_struct_1;
    return box.value;
}
