#define RECORD_COUNT 5
#define MIN_VALUE 6
struct Record10 { int key; int value; int active; };
const int checksum_bias_10 = 5;
static int kept_records_10 = 0;
static int filter_records_10(struct Record10 *records) {
    int total = 0;
    for (int i = 0; i < RECORD_COUNT; ++i) {
        if (records[i].active && records[i].value >= MIN_VALUE) {
            total += records[i].key + records[i].value + checksum_bias_10;
            kept_records_10 += 1;
        }
    }
    return total;
}
int main(void) {
    struct Record10 records[RECORD_COUNT] = {{1,6,1},{2,9,1},{3,5,1},{4,11,0},{5,8,1}};
    int total = filter_records_10(records);
    if (kept_records_10 < 3) return 1;
    if (total <= 24) return 2;
    if (records[1].key != 2) return 3;
    return 0;
}
