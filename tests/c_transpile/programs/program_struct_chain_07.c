typedef struct Chain_07 {
    int left;
    int right;
} Chain_07;

static int chain_steps_07 = 0;

static int eval_chain_07(Chain_07 *items, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        items[i].right += i;
        if (items[i].left > items[i].right) total += items[i].left - items[i].right;
        else total += items[i].right - items[i].left;
        chain_steps_07 += 1;
    }
    return total;
}

int main(void) {
    Chain_07 items[6] = {{-4, 0}, {-1, 3}, {-5, -1}, {-4, 0}, {3, 7}, {-8, -4}};
    int score = eval_chain_07(items, 6);
    if (chain_steps_07 != 6) return 1;
    if (score != 39) return 2;
    return 0;
}
