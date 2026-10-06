static int churn_moves_06 = 0;

static int churn_once_06(int *data, int n) {
    int first = data[0];
    for (int i = 0; i + 1 < n; ++i) {
        data[i] = data[i + 1] + (i % 2);
        churn_moves_06 += 1;
    }
    data[n - 1] = first;
    return data[0] + data[n - 1];
}

int main(void) {
    int data[9] = {2, -9, 3, -9, -5, 7, 7, 4, -2};
    int score = 0;
    score += churn_once_06(data, 9);
    score += churn_once_06(data, 9);
    if (churn_moves_06 != 16) return 1;
    if (score != -12) return 2;
    return 0;
}
