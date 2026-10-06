static int branch_hits_04 = 0;

static int mesh_eval_04(int value, int i) {
    int out = value;
    if ((value > 0 && i % 2 == 0) || (value < 0 && i % 3 == 0)) out = value + i;
    else if (value == 0 || i == 5) out = i - value;
    else out = value - i;
    branch_hits_04 += 1;
    return out;
}

int main(void) {
    int values[10] = {5, 6, 5, -9, 0, 6, -7, -9, -8, -9};
    int total = 0;
    for (int i = 0; i < 10; ++i) total += mesh_eval_04(values[i], i);
    if (branch_hits_04 != 10) return 1;
    if (total != -19) return 2;
    return 0;
}
