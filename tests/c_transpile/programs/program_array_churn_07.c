static int churn_moves_07 = 0;

static int churn_once_07(int *data, int n) {
    int first = data[0];
    for (int i = 0; i + 1 < n; ++i) {
        data[i] = data[i + 1] + (i % 2);
        churn_moves_07 += 1;
    }
    data[n - 1] = first;
    return data[0] + data[n - 1];
}

int main(void) {
    int data[9] = {10, 8, 2, -1, 8, 11, -7, 2, 10};
    int score = 0;
    score += churn_once_07(data, 9);
    score += churn_once_07(data, 9);
    if (churn_moves_07 != 16) return 1;
    if (score != 29) return 2;
    return 0;
}
