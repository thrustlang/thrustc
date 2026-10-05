int element_old(int *a, int i) {
    return a[i++];
}

int bump_element(int *a, int i) {
    a[i]++;
    return a[i];
}
