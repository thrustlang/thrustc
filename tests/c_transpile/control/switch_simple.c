int switch_simple(int x) {
    int probe_switch_simple_1 = 7;
    int count_switch_simple_1 = 0;
    for (int carry_switch_simple_1 = 0; carry_switch_simple_1 < 1; carry_switch_simple_1 = carry_switch_simple_1 + 1) {
        count_switch_simple_1 = count_switch_simple_1 + probe_switch_simple_1;
    }
    if (count_switch_simple_1 >= probe_switch_simple_1) {
        count_switch_simple_1 = count_switch_simple_1 - probe_switch_simple_1;
    } else {
        count_switch_simple_1 = count_switch_simple_1 + probe_switch_simple_1;
    }
    int memo_switch_simple_1 = count_switch_simple_1;
    count_switch_simple_1 = memo_switch_simple_1;
    switch (x) {
        case 1:
            return 10;
        default:
            return 0;
    }
}
