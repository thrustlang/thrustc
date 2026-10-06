const int flux_anchor_07 = 4;
static int flux_visits_07 = 0;

static int fold_flux_07(const int *values, int start, int count) {
    int total = 0;
    for (int i = start; i < start + count; ++i) {
        int sample = values[i];
        if ((i % 2) == 0) {
            total += sample + flux_anchor_07;
        } else {
            total += sample - 5;
        }
        flux_visits_07 += 1;
    }
    return total;
}

int main(void) {
    int data[10] = {-8, 10, -4, 0, -4, -2, 7, -8, 11, -3};
    int left = fold_flux_07(data, 0, 6);
    int right = fold_flux_07(data, 3, 7);
    int mixed = left + right;

    if (flux_visits_07 != 13) return 1;
    if (mixed != -18) return 2;
    return 0;
}
