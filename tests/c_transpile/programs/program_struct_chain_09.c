typedef struct Chain_09 {
    int left;
    int right;
} Chain_09;

static int chain_steps_09 = 0;

static int eval_chain_09(Chain_09 *items, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        items[i].right += i;
        if (items[i].left > items[i].right) total += items[i].left - items[i].right;
        else total += items[i].right - items[i].left;
        chain_steps_09 += 1;
    }
    return total;
}

int main(void) {
    Chain_09 items[6] = {{5, 7}, {-9, -7}, {-9, -7}, {0, 2}, {-7, -5}, {9, 11}};
    int score = eval_chain_09(items, 6);
    if (chain_steps_09 != 6) return 1;
    if (score != 27) return 2;
    return 0;
}
