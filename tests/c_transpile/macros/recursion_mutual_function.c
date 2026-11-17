int MUTUAL_A(int x) {
    return x + 1;
}

int MUTUAL_B(int x) {
    return MUTUAL_A(x) + 2;
}

#define MUTUAL_A(x) MUTUAL_B(x)
#define MUTUAL_B(x) MUTUAL_A(x)

int recursion_mutual(int x) {
    return MUTUAL_A(x);
}
