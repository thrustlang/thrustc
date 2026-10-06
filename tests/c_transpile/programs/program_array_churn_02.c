static int churn_moves_02 = 0;

static int churn_once_02(int *data, int n) {
    int first = data[0];
    for (int i = 0; i + 1 < n; ++i) {
        data[i] = data[i + 1] + (i % 2);
        churn_moves_02 += 1;
    }
    data[n - 1] = first;
    return data[0] + data[n - 1];
}

int main(void) {
    int data[9] = {8, 7, -9, -7, 12, 9, 12, 0, 5};
    int score = 0;
    score += churn_once_02(data, 9);
    score += churn_once_02(data, 9);
    if (churn_moves_02 != 16) return 1;
    if (score != 14) return 2;
    return 0;
}
