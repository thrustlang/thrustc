#define CHECK_4(x) if ((x) > 4) return 4
#define LEAVE_4(x) if ((x) > 4) break
#define SKIP_4(x) if (((x) % 2) == 0) continue

int test_return_4(int x) {
    CHECK_4(x);
    return x;
}

int test_break_4(void) {
    int i;
    for (i = 0; i < 10; i++) {
        LEAVE_4(i);
    }
    return i;
}

int test_continue_4(void) {
    int i;
    int total = 0;
    for (i = 0; i < 8; i++) {
        SKIP_4(i);
        total += i;
    }
    return total;
}

int main(void) {
    if (test_return_4(10) != (10 > 4 ? 4 : 10)) return 1;
    if (test_break_4() != (4 + 1 < 10 ? 4 + 1 : 10)) return 2;
    if (test_continue_4() != 16) return 3;
    return 0;
}
