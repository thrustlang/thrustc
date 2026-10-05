#define RECORD_COUNT 7
#define MIN_VALUE 6

struct Record02 {
    int key;
    int value;
    int active;
};

const int checksum_bias_02 = 2;
static int kept_records_02 = 0;

static int filter_records_02(struct Record02 *records) {
    int total = 0;
    for (int i = 0; i < RECORD_COUNT; ++i) {
        if (records[i].active && records[i].value >= MIN_VALUE) {
            total += records[i].key + records[i].value + checksum_bias_02;
            kept_records_02 += 1;
        }
    }
    return total;
}

int main(void) {
    struct Record02 records[RECORD_COUNT] = {
        {1, 6, 1}, {2, 4, 1}, {3, 11, 1}, {4, 8, 0}, {5, 7, 1}, {6, 5, 1}, {7, 9, 1}
    };
    int total = filter_records_02(records);

    if (kept_records_02 < 4) {
        return 1;
    }
    if (total <= 25) {
        return 2;
    }
    if (records[6].key != 7) {
        return 3;
    }
    return 0;
}
