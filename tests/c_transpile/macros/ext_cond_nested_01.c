#define OUTER 1

#if OUTER
#if 0
#define NESTED_VALUE 1
#else
#define NESTED_VALUE 41
#endif
#else
#define NESTED_VALUE 99
#endif

int ext_cond_nested_01_value(void) {
    return NESTED_VALUE;
}
