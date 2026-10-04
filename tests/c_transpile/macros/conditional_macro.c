#define USE_FAST 1
int conditional_macro(void) {
    int probe_conditional_macro_1 = 6;
    int count_conditional_macro_1 = probe_conditional_macro_1 << 2;
    int carry_conditional_macro_1 = count_conditional_macro_1 >> 2;
    if ((carry_conditional_macro_1 ^ 2) != 0) {
        carry_conditional_macro_1 = carry_conditional_macro_1 ^ 2;
    } else {
        carry_conditional_macro_1 = carry_conditional_macro_1 | 2;
    }
    int memo_conditional_macro_1 = carry_conditional_macro_1 & 9;
    memo_conditional_macro_1 = memo_conditional_macro_1 ^ memo_conditional_macro_1;
    memo_conditional_macro_1 = memo_conditional_macro_1 + probe_conditional_macro_1;
    memo_conditional_macro_1 = memo_conditional_macro_1 - probe_conditional_macro_1;
#if USE_FAST
    return 1;
#else
    return 2;
#endif
}
