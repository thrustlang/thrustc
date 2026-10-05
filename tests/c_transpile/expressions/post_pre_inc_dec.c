int post_pre_inc_dec(int start) {
    int x_post_pre_inc_dec_1 = start++;
    int y_post_pre_inc_dec_1 = ++start;
    start++;
    --start;
    return x_post_pre_inc_dec_1 + y_post_pre_inc_dec_1 + start;
}
