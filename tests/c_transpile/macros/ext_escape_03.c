#define CHECK_3(x) if ((x) > 6) return 3
#define LEAVE_3(x) if ((x) > 6) break
#define SKIP_3(x) if (((x) % 2) == 0) continue

int test_return_3(int x) {
    CHECK_3(x);
    return x;
}

int test_break_3(void) {
    int i;
    for (i = 0; i < 10; i++) {
        LEAVE_3(i);
    }
    return i;
}

int test_continue_3(void) {
    int i;
    int total = 0;
    for (i = 0; i < 8; i++) {
        SKIP_3(i);
        total += i;
    }
    return total;
}

int main(void) {
    if (test_return_3(2) != (2 > 6 ? 3 : 2)) return 1;
    if (test_break_3() != (6 + 1 < 10 ? 6 + 1 : 10)) return 2;
    if (test_continue_3() != 16) return 3;
    return 0;
}
