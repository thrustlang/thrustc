static int cast_steps_10 = 0;

static int ladder_cast_10(const int *values, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        int base = values[i];
        unsigned int up = (unsigned int)(base + i + 17);
        int down = (int)(up % 19u);
        total += down - (base / 3);
        cast_steps_10 += 1;
    }
    return total;
}

int main(void) {
    int values[8] = {-2, -6, -5, -5, -3, -1, -8, 10};
    int total = ladder_cast_10(values, 8);
    if (cast_steps_10 != 8) return 1;
    if (total != 110) return 2;
    return 0;
}
