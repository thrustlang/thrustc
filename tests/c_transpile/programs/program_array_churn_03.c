static int churn_moves_03 = 0;

static int churn_once_03(int *data, int n) {
    int first = data[0];
    for (int i = 0; i + 1 < n; ++i) {
        data[i] = data[i + 1] + (i % 2);
        churn_moves_03 += 1;
    }
    data[n - 1] = first;
    return data[0] + data[n - 1];
}

int main(void) {
    int data[9] = {12, -6, 6, -7, -1, 3, 0, -5, 9};
    int score = 0;
    score += churn_once_03(data, 9);
    score += churn_once_03(data, 9);
    if (churn_moves_03 != 16) return 1;
    if (score != 7) return 2;
    return 0;
}
