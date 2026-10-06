static int cast_steps_01 = 0;

static int ladder_cast_01(const int *values, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        int base = values[i];
        unsigned int up = (unsigned int)(base + i + 17);
        int down = (int)(up % 19u);
        total += down - (base / 3);
        cast_steps_01 += 1;
    }
    return total;
}

int main(void) {
    int values[8] = {9, 10, 0, -3, 9, 12, 4, 5};
    int total = ladder_cast_01(values, 8);
    if (cast_steps_01 != 8) return 1;
    if (total != 63) return 2;
    return 0;
}
