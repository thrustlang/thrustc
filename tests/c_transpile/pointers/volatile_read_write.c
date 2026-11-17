volatile int global_flag = 3;

int volatile_read(void) {
    volatile int local = 5;
    volatile int *p = &local;
    return *p;
}

int volatile_write(int value) {
    volatile int local = 0;
    volatile int *p = &local;
    *p = value;
    return local;
}

int volatile_global(void) {
    return global_flag;
}
