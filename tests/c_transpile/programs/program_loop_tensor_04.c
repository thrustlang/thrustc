static int loop_hits_04 = 0;

static int walk_tensor_04(int *flat) {
    int total = 0;
    for (int row = 0; row < 3; ++row) {
        for (int col = 0; col < 4; ++col) {
            int k = row * 4 + col;
            int v = flat[k];
            if ((v % 2) == 0) total += v + row;
            else total -= v - col;
            loop_hits_04 += 1;
        }
    }
    return total;
}

int main(void) {
    int data[12] = {7, -1, 6, 6, 1, 0, 4, 10, -8, 12, -6, -1};
    int total = walk_tensor_04(data);
    if (loop_hits_04 != 12) return 1;
    if (total != 31) return 2;
    return 0;
}
