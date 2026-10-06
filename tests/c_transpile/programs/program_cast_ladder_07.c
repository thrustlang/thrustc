static int cast_steps_07 = 0;

static int ladder_cast_07(const int *values, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        int base = values[i];
        unsigned int up = (unsigned int)(base + i + 17);
        int down = (int)(up % 19u);
        total += down - (base / 3);
        cast_steps_07 += 1;
    }
    return total;
}

int main(void) {
    int values[8] = {5, -7, -5, 4, 9, -3, 3, -6};
    int total = ladder_cast_07(values, 8);
    if (cast_steps_07 != 8) return 1;
    if (total != 69) return 2;
    return 0;
}
