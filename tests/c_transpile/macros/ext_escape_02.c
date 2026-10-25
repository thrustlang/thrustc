#define CHECK_2(x) if ((x) > 5) return 2
#define LEAVE_2(x) if ((x) > 5) break
#define SKIP_2(x) if (((x) % 2) == 0) continue

int test_return_2(int x) {
    CHECK_2(x);
    return x;
}

int test_break_2(void) {
    int i;
    for (i = 0; i < 10; i++) {
        LEAVE_2(i);
    }
    return i;
}

int test_continue_2(void) {
    int i;
    int total = 0;
    for (i = 0; i < 8; i++) {
        SKIP_2(i);
        total += i;
    }
    return total;
}

int main(void) {
    if (test_return_2(7) != (7 > 5 ? 2 : 7)) return 1;
    if (test_break_2() != (5 + 1 < 10 ? 5 + 1 : 10)) return 2;
    if (test_continue_2() != 16) return 3;
    return 0;
}
