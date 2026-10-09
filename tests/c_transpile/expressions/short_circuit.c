/* short_circuit.c: logical operator coverage without evaluation-order dependence. */

static int safe_div(int a, int b) {
    if (b == 0) return 0;
    return a / b > 1;
}

int short_circuit_check(void) {
    int a = 4;
    int b = 0;
    int r1 = (a > 0) && (a < 10);
    int r2 = (b != 0) && (a > 0);
    int r3 = (a < 0) || (b == 0);
    int r4 = (b > 0) || (a == 4);
    int div_ok = safe_div(a, b);
    int div_one = safe_div(a, 2);
    int chain = (a > 0) && (b == 0) && (a + b > 0);
    int orchain = (a < 0) || (b > 0) || (a == 4);
    int mixed = ((a > b) && (b < a)) || ((a % 2) == 0);

    if (r1 != 1) return 1;
    if (r2 != 0) return 2;
    if (r3 != 1) return 3;
    if (r4 != 1) return 4;
    if (div_ok != 0) return 5;
    if (div_one != 1) return 6;
    if (chain != 1) return 7;
    if (orchain != 1) return 8;
    if (mixed != 1) return 9;

    return 0;
}
