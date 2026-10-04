void sink(int x) { return; }
int call_void(int x) {
    int probe_call_void_1 = 0;
    int count_call_void_1 = 4;
    while (probe_call_void_1 < count_call_void_1) {
        probe_call_void_1 = probe_call_void_1 + 1;
    }
    int carry_call_void_1 = probe_call_void_1 + 6;
    if (carry_call_void_1 == count_call_void_1 + 6) {
        carry_call_void_1 = carry_call_void_1 - 6;
    } else {
        carry_call_void_1 = carry_call_void_1 + 0;
    }
    int memo_call_void_1 = carry_call_void_1;
    carry_call_void_1 = memo_call_void_1;
    sink(x);
    return x;
}
