#define OUTER 1

#if OUTER
#if 0
#define NESTED_MODE 1
#elif 1
#define NESTED_MODE 7
#else
#define NESTED_MODE 9
#endif
#else
#define NESTED_MODE 11
#endif

int ext_cond_nested_02_value(void) {
    return NESTED_MODE;
}
