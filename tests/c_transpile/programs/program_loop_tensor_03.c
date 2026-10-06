static int loop_hits_03 = 0;

static int walk_tensor_03(int *flat) {
    int total = 0;
    for (int row = 0; row < 3; ++row) {
        for (int col = 0; col < 4; ++col) {
            int k = row * 4 + col;
            int v = flat[k];
            if ((v % 2) == 0) total += v + row;
            else total -= v - col;
            loop_hits_03 += 1;
        }
    }
    return total;
}

int main(void) {
    int data[12] = {0, 0, 11, 9, -1, -3, 3, 1, 5, -8, -8, -7};
    int total = walk_tensor_03(data);
    if (loop_hits_03 != 12) return 1;
    if (total != -16) return 2;
    return 0;
}
