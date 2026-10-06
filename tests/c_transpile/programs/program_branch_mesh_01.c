static int branch_hits_01 = 0;

static int mesh_eval_01(int value, int i) {
    int out = value;
    if ((value > 0 && i % 2 == 0) || (value < 0 && i % 3 == 0)) out = value + i;
    else if (value == 0 || i == 5) out = i - value;
    else out = value - i;
    branch_hits_01 += 1;
    return out;
}

int main(void) {
    int values[10] = {-8, 0, -1, -7, 9, -1, 9, 10, 7, -8};
    int total = 0;
    for (int i = 0; i < 10; ++i) total += mesh_eval_01(values[i], i);
    if (branch_hits_01 != 10) return 1;
    if (total != 39) return 2;
    return 0;
}
