#define STAGES 3
#define BASE 10
#define OFFSET 5
#define ADD(a, b) ((a) + (b))
#define MUL(a, b) ((a) * (b))
#define CHAIN(x) ADD(MUL((x), BASE), OFFSET)
#define APPLY_STAGE(acc, st) \
    do { \
        (acc) = CHAIN((acc) + (st)); \
    } while (0)
#define CHECK_AND_SET(cond, slot, val) \
    do { \
        if (cond) { \
            (slot) = (val); \
        } \
    } while (0)
#if STAGES >= 3
#define FULL_PIPELINE 1
#else
#define FULL_PIPELINE 0
#endif
#if FULL_PIPELINE
#define PIPE_LABEL 99
#else
#define PIPE_LABEL 0
#endif
#ifndef BASE
#define BASE 1
#endif

static int stage_once(int value) {
    int acc = value;
    APPLY_STAGE(acc, 1);
    return acc;
}

static int pipeline(int seed, int *log) {
    int acc = seed;
    for (int s = 0; s < STAGES; s++) {
        APPLY_STAGE(acc, s);
        CHECK_AND_SET(acc > 100, log[s], acc);
    }
    return acc;
}

int macro_pipeline_steps_check(void) {
    int log[STAGES] = { 0, 0, 0 };
    int result = pipeline(1, log);
    if (FULL_PIPELINE != 1 || PIPE_LABEL != 99) { return 1; }
    if (stage_once(0) != CHAIN(0 + 1)) { return 2; }
    if (result <= 0) { return 3; }
    if (log[STAGES - 1] == 0 && result > 100) { return 4; }
    return 0;
}
