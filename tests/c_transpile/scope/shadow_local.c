int shadow_local(int x) {
    int probe_shadow_local_1 = 6;
    int count_shadow_local_1 = 4;
    int carry_shadow_local_1 = probe_shadow_local_1 + count_shadow_local_1;
    if (carry_shadow_local_1 > count_shadow_local_1) {
        carry_shadow_local_1 = carry_shadow_local_1 - count_shadow_local_1;
    } else {
        carry_shadow_local_1 = carry_shadow_local_1 + count_shadow_local_1;
    }
    int memo_shadow_local_1 = carry_shadow_local_1;
    {
        int edge_shadow_local_1 = memo_shadow_local_1;
        memo_shadow_local_1 = edge_shadow_local_1;
    }
    int y = x;
    {
        int y = 3;
        x = y;
    }
    return x + y;
}
