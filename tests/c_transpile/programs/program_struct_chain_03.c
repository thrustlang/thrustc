typedef struct Chain_03 {
    int left;
    int right;
} Chain_03;

static int chain_steps_03 = 0;

static int eval_chain_03(Chain_03 *items, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        items[i].right += i;
        if (items[i].left > items[i].right) total += items[i].left - items[i].right;
        else total += items[i].right - items[i].left;
        chain_steps_03 += 1;
    }
    return total;
}

int main(void) {
    Chain_03 items[6] = {{12, 16}, {-2, 2}, {-2, 2}, {-5, -1}, {-8, -4}, {-6, -2}};
    int score = eval_chain_03(items, 6);
    if (chain_steps_03 != 6) return 1;
    if (score != 39) return 2;
    return 0;
}
