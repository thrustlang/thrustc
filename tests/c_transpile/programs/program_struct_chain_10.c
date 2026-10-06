typedef struct Chain_10 {
    int left;
    int right;
} Chain_10;

static int chain_steps_10 = 0;

static int eval_chain_10(Chain_10 *items, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        items[i].right += i;
        if (items[i].left > items[i].right) total += items[i].left - items[i].right;
        else total += items[i].right - items[i].left;
        chain_steps_10 += 1;
    }
    return total;
}

int main(void) {
    Chain_10 items[6] = {{-5, -2}, {-1, 2}, {7, 10}, {0, 3}, {11, 14}, {12, 15}};
    int score = eval_chain_10(items, 6);
    if (chain_steps_10 != 6) return 1;
    if (score != 33) return 2;
    return 0;
}
