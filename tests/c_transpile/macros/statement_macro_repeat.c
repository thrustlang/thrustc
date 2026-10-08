#define REPEAT3(x) do { (x) = (x) + 1; (x) = (x) + 1; (x) = (x) + 1; } while (0)
int repeat_three(void) {
    int k = 0;
    REPEAT3(k);
    return k;
}
