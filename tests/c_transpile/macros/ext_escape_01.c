#define CHECK_1(x) if ((x) > 5) return 1
#define LEAVE_1(x) if ((x) > 5) break
#define SKIP_1(x) if (((x) % 2) == 0) continue

int test_return_1(int x) {
    CHECK_1(x);
    return x;
}

int test_break_1(void) {
    int i;
    for (i = 0; i < 10; i++) {
        LEAVE_1(i);
    }
    return i;
}

int test_continue_1(void) {
    int i;
    int total = 0;
    for (i = 0; i < 8; i++) {
        SKIP_1(i);
        total += i;
    }
    return total;
}

int main(void) {
    if (test_return_1(3) != (3 > 5 ? 1 : 3)) return 1;
    if (test_break_1() != (5 + 1 < 10 ? 5 + 1 : 10)) return 2;
    if (test_continue_1() != 16) return 3;
    return 0;
}
