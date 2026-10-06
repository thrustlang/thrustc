const int flux_anchor_09 = 6;
static int flux_visits_09 = 0;

static int fold_flux_09(const int *values, int start, int count) {
    int total = 0;
    for (int i = start; i < start + count; ++i) {
        int sample = values[i];
        if ((i % 2) == 0) {
            total += sample + flux_anchor_09;
        } else {
            total += sample - 3;
        }
        flux_visits_09 += 1;
    }
    return total;
}

int main(void) {
    int data[10] = {-4, -9, 6, -7, 7, 6, -4, 7, -2, -3};
    int left = fold_flux_09(data, 0, 6);
    int right = fold_flux_09(data, 3, 7);
    int mixed = left + right;

    if (flux_visits_09 != 13) return 1;
    if (mixed != 18) return 2;
    return 0;
}
