#define FILL2(p) do { (p)[0] = 10; (p)[1] = 20; } while (0)
int fill_sum(void) {
    int arr[2];
    arr[0] = 0;
    arr[1] = 0;
    FILL2(arr);
    return arr[0] + arr[1];
}
