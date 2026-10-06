const int flux_anchor_10 = 7;
static int flux_visits_10 = 0;

static int fold_flux_10(const int *values, int start, int count) {
    int total = 0;
    for (int i = start; i < start + count; ++i) {
        int sample = values[i];
        if ((i % 2) == 0) {
            total += sample + flux_anchor_10;
        } else {
            total += sample - 4;
        }
        flux_visits_10 += 1;
    }
    return total;
}

int main(void) {
    int data[10] = {12, 10, -2, 8, -7, 4, 4, 4, -4, -5};
    int left = fold_flux_10(data, 0, 6);
    int right = fold_flux_10(data, 3, 7);
    int mixed = left + right;

    if (flux_visits_10 != 13) return 1;
    if (mixed != 43) return 2;
    return 0;
}
