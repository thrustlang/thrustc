static int cast_steps_04 = 0;

static int ladder_cast_04(const int *values, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        int base = values[i];
        unsigned int up = (unsigned int)(base + i + 17);
        int down = (int)(up % 19u);
        total += down - (base / 3);
        cast_steps_04 += 1;
    }
    return total;
}

int main(void) {
    int values[8] = {6, 7, -4, -3, 12, 12, 12, 9};
    int total = ladder_cast_04(values, 8);
    if (cast_steps_04 != 8) return 1;
    if (total != 84) return 2;
    return 0;
}
