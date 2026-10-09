static int global_shadow_total = 3;
static int global_shadow_scale = 4;

static int fold_global_shadow(int value) {
    int scale = value + 1;
    global_shadow_total += scale;
    {
        int global_shadow_total = scale * 2;
        global_shadow_total += value;
        scale = global_shadow_total;
    }
    return scale + global_shadow_total;
}

static int rebind_names(int seed) {
    int value = seed;
    {
        int value = seed + 1;
        {
            int value = seed + 2;
            seed = value;
        }
        seed += value;
    }
    value += seed;
    return value;
}

static int order_probe(int a, int b) {
    int first = a + b;
    int second = first * 2;
    int third = second - a;
    {
        int first = third + b;
        second = first + second;
    }
    return first + second + third;
}

int global_shadow_check(void) {
    global_shadow_total = 3;
    global_shadow_scale = 4;
    if (global_shadow_total != 3) { return 1; }
    if (global_shadow_scale != 4) { return 2; }
    if (fold_global_shadow(5) != 26) { return 3; }
    if (global_shadow_total != 9) { return 4; }
    if (global_shadow_scale != 4) { return 5; }
    if (rebind_names(2) != 9) { return 6; }
    if (order_probe(3, 4) != 47) { return 7; }
    return 0;
}
