#define TEN 10
int object_macro(void) {
    int probe_object_macro_1 = 6;
    int count_object_macro_1 = probe_object_macro_1 << 4;
    int carry_object_macro_1 = count_object_macro_1 >> 4;
    if ((carry_object_macro_1 ^ 6) != 0) {
        carry_object_macro_1 = carry_object_macro_1 ^ 6;
    } else {
        carry_object_macro_1 = carry_object_macro_1 | 6;
    }
    int memo_object_macro_1 = carry_object_macro_1 & 6;
    memo_object_macro_1 = memo_object_macro_1 ^ memo_object_macro_1;
    memo_object_macro_1 = memo_object_macro_1 + probe_object_macro_1;
    memo_object_macro_1 = memo_object_macro_1 - probe_object_macro_1;
    return TEN;
}
