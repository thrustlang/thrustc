static int churn_moves_04 = 0;

static int churn_once_04(int *data, int n) {
    int first = data[0];
    for (int i = 0; i + 1 < n; ++i) {
        data[i] = data[i + 1] + (i % 2);
        churn_moves_04 += 1;
    }
    data[n - 1] = first;
    return data[0] + data[n - 1];
}

int main(void) {
    int data[9] = {-5, 10, -6, -2, -3, -3, 2, -7, 12};
    int score = 0;
    score += churn_once_04(data, 9);
    score += churn_once_04(data, 9);
    if (churn_moves_04 != 16) return 1;
    if (score != 10) return 2;
    return 0;
}
