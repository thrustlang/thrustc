typedef struct Chain_02 {
    int left;
    int right;
} Chain_02;

static int chain_steps_02 = 0;

static int eval_chain_02(Chain_02 *items, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        items[i].right += i;
        if (items[i].left > items[i].right) total += items[i].left - items[i].right;
        else total += items[i].right - items[i].left;
        chain_steps_02 += 1;
    }
    return total;
}

int main(void) {
    Chain_02 items[6] = {{11, 14}, {4, 7}, {5, 8}, {-7, -4}, {-9, -6}, {-2, 1}};
    int score = eval_chain_02(items, 6);
    if (chain_steps_02 != 6) return 1;
    if (score != 33) return 2;
    return 0;
}
