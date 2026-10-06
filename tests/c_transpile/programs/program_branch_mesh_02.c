static int branch_hits_02 = 0;

static int mesh_eval_02(int value, int i) {
    int out = value;
    if ((value > 0 && i % 2 == 0) || (value < 0 && i % 3 == 0)) out = value + i;
    else if (value == 0 || i == 5) out = i - value;
    else out = value - i;
    branch_hits_02 += 1;
    return out;
}

int main(void) {
    int values[10] = {-1, 11, 2, 11, -5, 3, 1, 9, 9, -2};
    int total = 0;
    for (int i = 0; i < 10; ++i) total += mesh_eval_02(values[i], i);
    if (branch_hits_02 != 10) return 1;
    if (total != 47) return 2;
    return 0;
}
