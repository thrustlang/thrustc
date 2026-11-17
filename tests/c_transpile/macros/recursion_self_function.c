int RECURSE(int x) {
    return x + 1;
}

#define RECURSE(x) ((x) + RECURSE(x))

int recursion_function(int x) {
    return RECURSE(x);
}
