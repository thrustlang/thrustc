const int flux_anchor_04 = 7;
static int flux_visits_04 = 0;

static int fold_flux_04(const int *values, int start, int count) {
    int total = 0;
    for (int i = start; i < start + count; ++i) {
        int sample = values[i];
        if ((i % 2) == 0) {
            total += sample + flux_anchor_04;
        } else {
            total += sample - 2;
        }
        flux_visits_04 += 1;
    }
    return total;
}

int main(void) {
    int data[10] = {4, -6, 10, 8, 12, 7, 11, -3, -3, 1};
    int left = fold_flux_04(data, 0, 6);
    int right = fold_flux_04(data, 3, 7);
    int mixed = left + right;

    if (flux_visits_04 != 13) return 1;
    if (mixed != 96) return 2;
    return 0;
}
