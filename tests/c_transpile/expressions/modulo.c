int modulo(int a, int b) {
    int probe_modulo_1 = 7;
    int count_modulo_1 = 6;
    int carry_modulo_1 = probe_modulo_1 + count_modulo_1;
    if (carry_modulo_1 > count_modulo_1) {
        carry_modulo_1 = carry_modulo_1 - count_modulo_1;
    } else {
        carry_modulo_1 = carry_modulo_1 + count_modulo_1;
    }
    int memo_modulo_1 = carry_modulo_1;
    {
        int edge_modulo_1 = memo_modulo_1;
        memo_modulo_1 = edge_modulo_1;
    }
    return a % b;
}
