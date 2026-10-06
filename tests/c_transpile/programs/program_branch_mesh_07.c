static int branch_hits_07 = 0;

static int mesh_eval_07(int value, int i) {
    int out = value;
    if ((value > 0 && i % 2 == 0) || (value < 0 && i % 3 == 0)) out = value + i;
    else if (value == 0 || i == 5) out = i - value;
    else out = value - i;
    branch_hits_07 += 1;
    return out;
}

int main(void) {
    int values[10] = {1, -6, -3, -3, 11, 2, -9, 3, 2, -5};
    int total = 0;
    for (int i = 0; i < 10; ++i) total += mesh_eval_07(values[i], i);
    if (branch_hits_07 != 10) return 1;
    if (total != 14) return 2;
    return 0;
}
