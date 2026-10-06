static int branch_hits_06 = 0;

static int mesh_eval_06(int value, int i) {
    int out = value;
    if ((value > 0 && i % 2 == 0) || (value < 0 && i % 3 == 0)) out = value + i;
    else if (value == 0 || i == 5) out = i - value;
    else out = value - i;
    branch_hits_06 += 1;
    return out;
}

int main(void) {
    int values[10] = {-7, 3, 1, -4, -9, 6, -6, 6, 10, 12};
    int total = 0;
    for (int i = 0; i < 10; ++i) total += mesh_eval_06(values[i], i);
    if (branch_hits_06 != 10) return 1;
    if (total != 3) return 2;
    return 0;
}
