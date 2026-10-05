#define RECORD_COUNT 6
#define MIN_VALUE 7

struct Record01 {
    int key;
    int value;
    int active;
};

const int checksum_bias_01 = 3;
static int kept_records_01 = 0;

static int filter_records_01(struct Record01 *records) {
    int total = 0;
    for (int i = 0; i < RECORD_COUNT; ++i) {
        if (records[i].active && records[i].value >= MIN_VALUE) {
            total += records[i].key + records[i].value + checksum_bias_01;
            kept_records_01 += 1;
        }
    }
    return total;
}

int main(void) {
    struct Record01 records[RECORD_COUNT] = {
        {1, 8, 1}, {2, 5, 1}, {3, 10, 0}, {4, 9, 1}, {5, 7, 1}, {6, 6, 0}
    };
    int total = filter_records_01(records);

    if (kept_records_01 < 3) {
        return 1;
    }
    if (total <= 20) {
        return 2;
    }
    if (records[0].key != 1) {
        return 3;
    }
    return 0;
}
