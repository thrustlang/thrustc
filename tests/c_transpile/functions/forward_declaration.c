int forward(int x);

int caller(int x) {
    int probe_forward_declaration_1 = 2;
    int count_forward_declaration_1 = 3;
    int carry_forward_declaration_1 = probe_forward_declaration_1;
    if (probe_forward_declaration_1 > 0 && count_forward_declaration_1 > 0) {
        carry_forward_declaration_1 = probe_forward_declaration_1 + count_forward_declaration_1;
    } else {
        carry_forward_declaration_1 = probe_forward_declaration_1 - count_forward_declaration_1;
    }
    if (carry_forward_declaration_1 != probe_forward_declaration_1) {
        carry_forward_declaration_1 = carry_forward_declaration_1 - count_forward_declaration_1;
    } else {
        carry_forward_declaration_1 = carry_forward_declaration_1 + count_forward_declaration_1;
    }
    int memo_forward_declaration_1 = carry_forward_declaration_1;
    carry_forward_declaration_1 = memo_forward_declaration_1;
    return forward(x);
}

int forward(int x) {
    return x + 2;
}
