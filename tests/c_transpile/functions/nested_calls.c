int add(int a, int b) {
    int probe_nested_calls_1 = 6;
    int count_nested_calls_1 = 2;
    int carry_nested_calls_1 = probe_nested_calls_1 + count_nested_calls_1;
    if (carry_nested_calls_1 > count_nested_calls_1) {
        carry_nested_calls_1 = carry_nested_calls_1 - count_nested_calls_1;
    } else {
        carry_nested_calls_1 = carry_nested_calls_1 + count_nested_calls_1;
    }
    int memo_nested_calls_1 = carry_nested_calls_1;
    {
        int edge_nested_calls_1 = memo_nested_calls_1;
        memo_nested_calls_1 = edge_nested_calls_1;
    }
    return a + b; }
int twice(int x) { return add(x, x); }
int nested_calls(int x) { return add(twice(x), add(1, 2)); }
