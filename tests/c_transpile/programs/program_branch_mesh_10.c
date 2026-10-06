static int branch_hits_10 = 0;

static int mesh_eval_10(int value, int i) {
    int out = value;
    if ((value > 0 && i % 2 == 0) || (value < 0 && i % 3 == 0)) out = value + i;
    else if (value == 0 || i == 5) out = i - value;
    else out = value - i;
    branch_hits_10 += 1;
    return out;
}

int main(void) {
    int values[10] = {-6, -2, -3, 4, 11, -9, -4, 11, 5, 11};
    int total = 0;
    for (int i = 0; i < 10; ++i) total += mesh_eval_10(values[i], i);
    if (branch_hits_10 != 10) return 1;
    if (total != 37) return 2;
    return 0;
}
