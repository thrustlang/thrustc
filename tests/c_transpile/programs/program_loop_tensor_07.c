static int loop_hits_07 = 0;

static int walk_tensor_07(int *flat) {
    int total = 0;
    for (int row = 0; row < 3; ++row) {
        for (int col = 0; col < 4; ++col) {
            int k = row * 4 + col;
            int v = flat[k];
            if ((v % 2) == 0) total += v + row;
            else total -= v - col;
            loop_hits_07 += 1;
        }
    }
    return total;
}

int main(void) {
    int data[12] = {2, 0, 9, 1, 4, 10, 3, 11, 7, 4, 2, -6};
    int total = walk_tensor_07(data);
    if (loop_hits_07 != 12) return 1;
    if (total != 3) return 2;
    return 0;
}
