#define CHANNEL_COUNT 6
#define WEIGHT_CAP 5
#define DRIFT_LIMIT 40

enum FusionState { FUSE_IDLE = 0, FUSE_ACCUM = 1, FUSE_READY = 2 };

struct Sample {
    int id;
    int raw;
    int weight;
};

const int fusion_bias = 7;
static int fused_frames = 0;

static int clamp_weight(int value) {
    if (value < 0) return 0;
    if (value > WEIGHT_CAP) return WEIGHT_CAP;
    return value;
}

static int fold_sample(struct Sample *sample, int drift) {
    int weight = clamp_weight(sample->weight + (drift % 3));
    int adjusted = (sample->raw * weight) - drift + fusion_bias;
    sample->weight = weight;
    fused_frames += 1;
    return adjusted;
}

static int fuse_all(struct Sample *samples, const int *drifts) {
    int total = 0;
    int state = FUSE_IDLE;
    for (int i = 0; i < CHANNEL_COUNT; ++i) {
        int value = fold_sample(&samples[i], drifts[i]);
        if (value > DRIFT_LIMIT) { state = FUSE_ACCUM; total += value; }
        else if (value > 0) { state = FUSE_READY; total += value / 2; }
        else { total -= fusion_bias; }
    }
    return state == FUSE_READY ? total + fusion_bias : total;
}

int sensor_fusion_check(void) {
    struct Sample samples[CHANNEL_COUNT] = {
        {1, 9, 2}, {2, 4, 5}, {3, 12, 1}, {4, 7, 3}, {5, 10, 4}, {6, 6, 0}
    };
    int drifts[CHANNEL_COUNT] = {3, 5, 1, 8, 2, 6};
    int total = fuse_all(samples, drifts);
    if (fused_frames != CHANNEL_COUNT) return 1;
    if (samples[1].weight > WEIGHT_CAP) return 2;
    if (total <= DRIFT_LIMIT) return 3;
    if (samples[0].raw != 9) return 4;
    return 0;
}
