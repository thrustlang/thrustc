typedef struct Chain_08 {
    int left;
    int right;
} Chain_08;

static int chain_steps_08 = 0;

static int eval_chain_08(Chain_08 *items, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        items[i].right += i;
        if (items[i].left > items[i].right) total += items[i].left - items[i].right;
        else total += items[i].right - items[i].left;
        chain_steps_08 += 1;
    }
    return total;
}

int main(void) {
    Chain_08 items[6] = {{7, 8}, {-6, -5}, {-1, 0}, {1, 2}, {-6, -5}, {-8, -7}};
    int score = eval_chain_08(items, 6);
    if (chain_steps_08 != 6) return 1;
    if (score != 21) return 2;
    return 0;
}
