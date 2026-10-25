#define CHECK_5(x) if ((x) > 3) return 5
#define LEAVE_5(x) if ((x) > 3) break
#define SKIP_5(x) if (((x) % 2) == 0) continue

int test_return_5(int x) {
    CHECK_5(x);
    return x;
}

int test_break_5(void) {
    int i;
    for (i = 0; i < 10; i++) {
        LEAVE_5(i);
    }
    return i;
}

int test_continue_5(void) {
    int i;
    int total = 0;
    for (i = 0; i < 8; i++) {
        SKIP_5(i);
        total += i;
    }
    return total;
}

int main(void) {
    if (test_return_5(1) != (1 > 3 ? 5 : 1)) return 1;
    if (test_break_5() != (3 + 1 < 10 ? 3 + 1 : 10)) return 2;
    if (test_continue_5() != 16) return 3;
    return 0;
}
