typedef struct Chain_05 {
    int left;
    int right;
} Chain_05;

static int chain_steps_05 = 0;

static int eval_chain_05(Chain_05 *items, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        items[i].right += i;
        if (items[i].left > items[i].right) total += items[i].left - items[i].right;
        else total += items[i].right - items[i].left;
        chain_steps_05 += 1;
    }
    return total;
}

int main(void) {
    Chain_05 items[6] = {{-2, 0}, {9, 11}, {9, 11}, {-6, -4}, {-5, -3}, {0, 2}};
    int score = eval_chain_05(items, 6);
    if (chain_steps_05 != 6) return 1;
    if (score != 27) return 2;
    return 0;
}
