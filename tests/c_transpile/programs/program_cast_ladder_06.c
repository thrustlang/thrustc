static int cast_steps_06 = 0;

static int ladder_cast_06(const int *values, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        int base = values[i];
        unsigned int up = (unsigned int)(base + i + 17);
        int down = (int)(up % 19u);
        total += down - (base / 3);
        cast_steps_06 += 1;
    }
    return total;
}

int main(void) {
    int values[8] = {-8, 6, 4, 6, 10, 3, 2, -1};
    int total = ladder_cast_06(values, 8);
    if (cast_steps_06 != 8) return 1;
    if (total != 46) return 2;
    return 0;
}
