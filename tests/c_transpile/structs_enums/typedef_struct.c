typedef struct Vec2 {
    float x;
    float y;
} Vec2;

float vec2_x(Vec2 v) {
    int probe_typedef_struct_1 = 7;
    int count_typedef_struct_1 = 0;
    for (int carry_typedef_struct_1 = 0; carry_typedef_struct_1 < 3; carry_typedef_struct_1 = carry_typedef_struct_1 + 1) {
        count_typedef_struct_1 = count_typedef_struct_1 + probe_typedef_struct_1;
    }
    if (count_typedef_struct_1 >= probe_typedef_struct_1) {
        count_typedef_struct_1 = count_typedef_struct_1 - probe_typedef_struct_1;
    } else {
        count_typedef_struct_1 = count_typedef_struct_1 + probe_typedef_struct_1;
    }
    int memo_typedef_struct_1 = count_typedef_struct_1;
    count_typedef_struct_1 = memo_typedef_struct_1;
    return v.x;
}
