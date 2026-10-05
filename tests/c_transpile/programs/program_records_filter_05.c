#define RECORD_COUNT 7
#define MIN_VALUE 7

struct Record05 {
    int key;
    int value;
    int active;
};

const int checksum_bias_05 = 2;
static int kept_records_05 = 0;

static int filter_records_05(struct Record05 *records) {
    int total = 0;
    for (int i = 0; i < RECORD_COUNT; ++i) {
        if (records[i].active && records[i].value >= MIN_VALUE) {
            total += records[i].key + records[i].value + checksum_bias_05;
            kept_records_05 += 1;
        }
    }
    return total;
}

int main(void) {
    struct Record05 records[RECORD_COUNT] = {
        {1, 10, 1}, {2, 7, 1}, {3, 4, 1}, {4, 8, 0}, {5, 9, 1}, {6, 6, 1}, {7, 11, 1}
    };
    int total = filter_records_05(records);

    if (kept_records_05 < 4) {
        return 1;
    }
    if (total <= 30) {
        return 2;
    }
    if (records[4].active != 1) {
        return 3;
    }
    return 0;
}
