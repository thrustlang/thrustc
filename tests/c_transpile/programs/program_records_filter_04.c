#define RECORD_COUNT 6
#define MIN_VALUE 5

struct Record04 {
    int key;
    int value;
    int active;
};

const int checksum_bias_04 = 1;
static int kept_records_04 = 0;

static int filter_records_04(struct Record04 *records) {
    int total = 0;
    for (int i = 0; i < RECORD_COUNT; ++i) {
        if (records[i].active && records[i].value >= MIN_VALUE) {
            total += records[i].key + records[i].value + checksum_bias_04;
            kept_records_04 += 1;
        }
    }
    return total;
}

int main(void) {
    struct Record04 records[RECORD_COUNT] = {
        {1, 5, 1}, {2, 4, 0}, {3, 8, 1}, {4, 9, 1}, {5, 6, 0}, {6, 7, 1}
    };
    int total = filter_records_04(records);

    if (kept_records_04 < 4) {
        return 1;
    }
    if (total <= 18) {
        return 2;
    }
    if (records[3].key != 4) {
        return 3;
    }
    return 0;
}
