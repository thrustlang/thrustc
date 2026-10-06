static int cast_steps_03 = 0;

static int ladder_cast_03(const int *values, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        int base = values[i];
        unsigned int up = (unsigned int)(base + i + 17);
        int down = (int)(up % 19u);
        total += down - (base / 3);
        cast_steps_03 += 1;
    }
    return total;
}

int main(void) {
    int values[8] = {-2, 6, 3, -1, -5, -1, 2, -6};
    int total = ladder_cast_03(values, 8);
    if (cast_steps_03 != 8) return 1;
    if (total != 65) return 2;
    return 0;
}
