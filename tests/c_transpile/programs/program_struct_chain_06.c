typedef struct Chain_06 {
    int left;
    int right;
} Chain_06;

static int chain_steps_06 = 0;

static int eval_chain_06(Chain_06 *items, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        items[i].right += i;
        if (items[i].left > items[i].right) total += items[i].left - items[i].right;
        else total += items[i].right - items[i].left;
        chain_steps_06 += 1;
    }
    return total;
}

int main(void) {
    Chain_06 items[6] = {{7, 10}, {-2, 1}, {12, 15}, {1, 4}, {-5, -2}, {12, 15}};
    int score = eval_chain_06(items, 6);
    if (chain_steps_06 != 6) return 1;
    if (score != 33) return 2;
    return 0;
}
