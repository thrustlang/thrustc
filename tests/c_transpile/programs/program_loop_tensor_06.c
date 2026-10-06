static int loop_hits_06 = 0;

static int walk_tensor_06(int *flat) {
    int total = 0;
    for (int row = 0; row < 3; ++row) {
        for (int col = 0; col < 4; ++col) {
            int k = row * 4 + col;
            int v = flat[k];
            if ((v % 2) == 0) total += v + row;
            else total -= v - col;
            loop_hits_06 += 1;
        }
    }
    return total;
}

int main(void) {
    int data[12] = {-8, -9, 3, -5, -8, 12, -9, -1, 5, 9, -9, 4};
    int total = walk_tensor_06(data);
    if (loop_hits_06 != 12) return 1;
    if (total != 34) return 2;
    return 0;
}
