typedef struct {
    int lo;
    int hi;
} Range;
typedef struct {
    int id;
    int reading;
    Range limits;
} Sensor;
typedef enum {
    STATE_LOW = 0,
    STATE_OK = 1,
    STATE_HIGH = 2,
} SensorState;

static SensorState sensor_state(Sensor s) {
    if (s.reading < s.limits.lo) return STATE_LOW;
    if (s.reading > s.limits.hi) return STATE_HIGH;
    return STATE_OK;
}
static void sensor_scale(Sensor *s, int factor) {
    s->reading = s->reading * factor;
    s->limits.lo = s->limits.lo * factor;
    s->limits.hi = s->limits.hi * factor;
}
static int grid_count_state(Sensor grid[], int n, SensorState want) {
    int count = 0;
    for (int i = 0; i < n; i++) {
        if (sensor_state(grid[i]) == want) count++;
    }
    return count;
}
static int grid_max_reading(const Sensor *grid, int n) {
    int best = grid[0].reading;
    for (int i = 1; i < n; i++) {
        if (grid[i].reading > best) best = grid[i].reading;
    }
    return best;
}

int sensor_grid_check(void) {
    Sensor grid[4] = {
        { 1, 5,  { 0, 10 } },
        { 2, 20, { 0, 10 } },
        { 3, -3, { 0, 10 } },
        { 4, 10, { 0, 10 } },
    };
    if (grid_count_state(grid, 4, STATE_OK) != 2) return 1;
    if (grid_count_state(grid, 4, STATE_HIGH) != 1) return 2;
    if (grid_count_state(grid, 4, STATE_LOW) != 1) return 3;
    if (grid_max_reading(grid, 4) != 20) return 4;
    sensor_scale(&grid[0], 3);
    if (grid[0].reading != 15) return 5;
    if (sensor_state(grid[0]) != STATE_OK) return 6;
    if (grid[3].limits.hi != 10) return 7;
    return 0;
}
