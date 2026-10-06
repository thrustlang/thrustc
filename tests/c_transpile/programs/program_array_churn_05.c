static int churn_moves_05 = 0;

static int churn_once_05(int *data, int n) {
    int first = data[0];
    for (int i = 0; i + 1 < n; ++i) {
        data[i] = data[i + 1] + (i % 2);
        churn_moves_05 += 1;
    }
    data[n - 1] = first;
    return data[0] + data[n - 1];
}

int main(void) {
    int data[9] = {4, 7, -1, 7, -8, -9, 11, -6, -1};
    int score = 0;
    score += churn_once_05(data, 9);
    score += churn_once_05(data, 9);
    if (churn_moves_05 != 16) return 1;
    if (score != 18) return 2;
    return 0;
}
