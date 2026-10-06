static int branch_hits_09 = 0;

static int mesh_eval_09(int value, int i) {
    int out = value;
    if ((value > 0 && i % 2 == 0) || (value < 0 && i % 3 == 0)) out = value + i;
    else if (value == 0 || i == 5) out = i - value;
    else out = value - i;
    branch_hits_09 += 1;
    return out;
}

int main(void) {
    int values[10] = {0, -2, 6, 11, -5, 4, 11, 9, 5, 11};
    int total = 0;
    for (int i = 0; i < 10; ++i) total += mesh_eval_09(values[i], i);
    if (branch_hits_09 != 10) return 1;
    if (total != 39) return 2;
    return 0;
}
