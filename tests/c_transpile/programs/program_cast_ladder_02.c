static int cast_steps_02 = 0;

static int ladder_cast_02(const int *values, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        int base = values[i];
        unsigned int up = (unsigned int)(base + i + 17);
        int down = (int)(up % 19u);
        total += down - (base / 3);
        cast_steps_02 += 1;
    }
    return total;
}

int main(void) {
    int values[8] = {8, -2, 3, 9, 6, 11, -5, -2};
    int total = ladder_cast_02(values, 8);
    if (cast_steps_02 != 8) return 1;
    if (total != 68) return 2;
    return 0;
}
