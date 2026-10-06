static int churn_moves_01 = 0;

static int churn_once_01(int *data, int n) {
    int first = data[0];
    for (int i = 0; i + 1 < n; ++i) {
        data[i] = data[i + 1] + (i % 2);
        churn_moves_01 += 1;
    }
    data[n - 1] = first;
    return data[0] + data[n - 1];
}

int main(void) {
    int data[9] = {-6, 1, 12, 2, 12, -5, 12, 2, -6};
    int score = 0;
    score += churn_once_01(data, 9);
    score += churn_once_01(data, 9);
    if (churn_moves_01 != 16) return 1;
    if (score != 9) return 2;
    return 0;
}
