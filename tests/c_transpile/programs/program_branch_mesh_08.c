static int branch_hits_08 = 0;

static int mesh_eval_08(int value, int i) {
    int out = value;
    if ((value > 0 && i % 2 == 0) || (value < 0 && i % 3 == 0)) out = value + i;
    else if (value == 0 || i == 5) out = i - value;
    else out = value - i;
    branch_hits_08 += 1;
    return out;
}

int main(void) {
    int values[10] = {10, 5, 4, 5, 1, 12, -1, -3, 12, 3};
    int total = 0;
    for (int i = 0; i < 10; ++i) total += mesh_eval_08(values[i], i);
    if (branch_hits_08 != 10) return 1;
    if (total != 29) return 2;
    return 0;
}
