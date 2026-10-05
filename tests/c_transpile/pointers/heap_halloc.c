void *malloc(unsigned long n);

int heap_halloc(void) {
    int *p = (int *)malloc(sizeof(int));
    *p = 41;
    double *q = (double *)malloc(4 * sizeof(double));
    q[0] = 1.0;
    int total = *p + (int)q[0];
    return total;
}
