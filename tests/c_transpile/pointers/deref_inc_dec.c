int bump_deref(int *p) {
    (*p)++;
    ++*p;
    return *p;
}

int fetch_old(int *p) {
    int old_deref_inc_dec_1 = (*p)++;
    return old_deref_inc_dec_1;
}
