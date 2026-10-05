#define RECORD_COUNT 5
#define MIN_VALUE 8

struct Record03 {
    int key;
    int value;
    int active;
};

const int checksum_bias_03 = 4;
static int kept_records_03 = 0;

static int filter_records_03(struct Record03 *records) {
    int total = 0;
    for (int i = 0; i < RECORD_COUNT; ++i) {
        if (records[i].active && records[i].value >= MIN_VALUE) {
            total += records[i].key + records[i].value + checksum_bias_03;
            kept_records_03 += 1;
        }
    }
    return total;
}

int main(void) {
    struct Record03 records[RECORD_COUNT] = {
        {1, 9, 1}, {2, 12, 1}, {3, 7, 0}, {4, 8, 1}, {5, 6, 1}
    };
    int total = filter_records_03(records);

    if (kept_records_03 < 3) {
        return 1;
    }
    if (total <= 20) {
        return 2;
    }
    if (records[1].value != 12) {
        return 3;
    }
    return 0;
}
