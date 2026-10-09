#define SECONDS_PER_MINUTE 60
#define MINUTES_PER_HOUR 60
#define HOURS_PER_DAY 24
#define SECONDS_PER_HOUR 3600
#define SECONDS_PER_DAY 86400
#define TO_SECONDS(min, sec) \
    ((min) * SECONDS_PER_MINUTE + (sec))
#define TO_MINUTES(sec) ((sec) / SECONDS_PER_MINUTE)
#define REMAINDER(sec) ((sec) % SECONDS_PER_MINUTE)
#define TOTAL_DAY_SECONDS(h, m, s) \
    ((h) * SECONDS_PER_HOUR + TO_SECONDS(m, s))
#if SECONDS_PER_DAY == 86400
#define DAY_OK 1
#else
#define DAY_OK 0
#endif
#define FITS_DAY(h, m, s) (TOTAL_DAY_SECONDS(h, m, s) < SECONDS_PER_DAY)
#define NORMALIZE(sec, slot) \
    do { \
        while ((sec) >= SECONDS_PER_DAY) { \
            (sec) = (sec) - SECONDS_PER_DAY; \
        } \
        (slot) = (sec); \
    } while (0)
#ifdef DAY_OK
#define CAL_TAG 7
#endif

static int day_seconds(unsigned int hours, unsigned int minutes, unsigned int secs) {
    return TOTAL_DAY_SECONDS(hours, minutes, secs);
}

int macro_units_convert_check(void) {
    unsigned int span;
    int norm;
    if (DAY_OK != 1 || CAL_TAG != 7) { return 1; }
    if (TO_SECONDS(2, 30) != 150) { return 2; }
    if (SECONDS_PER_HOUR != 3600) { return 3; }
    span = day_seconds(1, 0, 0);
    if (span != SECONDS_PER_HOUR) { return 4; }
    if (!FITS_DAY(23, 59, 59)) { return 5; }
    norm = SECONDS_PER_DAY + 5;
    NORMALIZE(norm, norm);
    if (norm != 5 || TO_MINUTES(150) != 2 || REMAINDER(150) != 30) {
        return 6;
    }
    return 0;
}
