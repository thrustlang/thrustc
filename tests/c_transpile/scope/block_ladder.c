static int base_value = 5;

static int bump_block_ladder(int seed) {
    int total = seed;
    {
        int step = base_value;
        total = total + step;
        {
            int step = 3;
            total = total * step;
        }
        total = total - step;
    }
    return total;
}

static int shadow_chain(int value) {
    int slot = value;
    if (slot > 0) {
        int half = slot / 2;
        int slot = half;
        if (slot > 1) {
            int down = slot - 1;
            int slot = down;
            value = slot;
        }
        slot = slot + 1;
        value = value + slot;
    }
    {
        int slot = 9;
        value = value ^ slot;
    }
    return value + slot;
}

static int sibling_blocks(int start) {
    int total = start;
    {
        int step = 2;
        total = step + step;
    }
    {
        int step = 10;
        total = total + step;
    }
    {
        int step = total - start;
        total = step - 1;
    }
    return total;
}

int block_ladder_check(void) {
    if (base_value != 5) { return 1; }
    if (bump_block_ladder(4) != 22) { return 2; }
    if (shadow_chain(20) != 49) { return 3; }
    if (sibling_blocks(7) != 6) { return 4; }
    return 0;
}
