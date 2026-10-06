typedef struct Chain_04 {
    int left;
    int right;
} Chain_04;

static int chain_steps_04 = 0;

static int eval_chain_04(Chain_04 *items, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        items[i].right += i;
        if (items[i].left > items[i].right) total += items[i].left - items[i].right;
        else total += items[i].right - items[i].left;
        chain_steps_04 += 1;
    }
    return total;
}

int main(void) {
    Chain_04 items[6] = {{6, 7}, {2, 3}, {6, 7}, {7, 8}, {-8, -7}, {3, 4}};
    int score = eval_chain_04(items, 6);
    if (chain_steps_04 != 6) return 1;
    if (score != 21) return 2;
    return 0;
}
