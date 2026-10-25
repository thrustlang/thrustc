#define FEATURE_PRESENT 1

#if 0
#define OUTER_VALUE 100
#else
#ifdef MISSING_FEATURE
#define OUTER_VALUE 5
#else
#define OUTER_VALUE 23
#endif
#endif

int ext_cond_nested_03_value(void) {
    return OUTER_VALUE;
}
