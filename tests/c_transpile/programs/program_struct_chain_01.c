typedef struct Chain_01 {
    int left;
    int right;
} Chain_01;

static int chain_steps_01 = 0;

static int eval_chain_01(Chain_01 *items, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        items[i].right += i;
        if (items[i].left > items[i].right) total += items[i].left - items[i].right;
        else total += items[i].right - items[i].left;
        chain_steps_01 += 1;
    }
    return total;
}

int main(void) {
    Chain_01 items[6] = {{4, 6}, {12, 14}, {-5, -3}, {4, 6}, {-3, -1}, {6, 8}};
    int score = eval_chain_01(items, 6);
    if (chain_steps_01 != 6) return 1;
    if (score != 27) return 2;
    return 0;
}
