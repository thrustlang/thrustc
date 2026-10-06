static int cast_steps_05 = 0;

static int ladder_cast_05(const int *values, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        int base = values[i];
        unsigned int up = (unsigned int)(base + i + 17);
        int down = (int)(up % 19u);
        total += down - (base / 3);
        cast_steps_05 += 1;
    }
    return total;
}

int main(void) {
    int values[8] = {-5, 8, 2, 3, -4, 3, 6, 9};
    int total = ladder_cast_05(values, 8);
    if (cast_steps_05 != 8) return 1;
    if (total != 65) return 2;
    return 0;
}
