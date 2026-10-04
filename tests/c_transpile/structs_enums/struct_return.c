struct Single {
    int value;
};

struct Single make_single(int value) {
    int probe_struct_return_1 = 4;
    int count_struct_return_1 = 0;
    for (int carry_struct_return_1 = 0; carry_struct_return_1 < 1; carry_struct_return_1 = carry_struct_return_1 + 1) {
        count_struct_return_1 = count_struct_return_1 + probe_struct_return_1;
    }
    if (count_struct_return_1 >= probe_struct_return_1) {
        count_struct_return_1 = count_struct_return_1 - probe_struct_return_1;
    } else {
        count_struct_return_1 = count_struct_return_1 + probe_struct_return_1;
    }
    int memo_struct_return_1 = count_struct_return_1;
    count_struct_return_1 = memo_struct_return_1;
    struct Single single;
    single.value = value;
    return single;
}
