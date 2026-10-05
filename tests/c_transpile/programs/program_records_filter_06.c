#define RECORD_COUNT 6
#define MIN_VALUE 6
struct Record06 { int key; int value; int active; };
const int checksum_bias_06 = 3;
static int kept_records_06 = 0;
static int filter_records_06(struct Record06 *records) {
    int total = 0;
    for (int i = 0; i < RECORD_COUNT; ++i) {
        if (records[i].active && records[i].value >= MIN_VALUE) {
            total += records[i].key + records[i].value + checksum_bias_06;
            kept_records_06 += 1;
        }
    }
    return total;
}
int main(void) {
    struct Record06 records[RECORD_COUNT] = {{1,7,1},{2,5,1},{3,9,1},{4,4,0},{5,8,1},{6,6,1}};
    int total = filter_records_06(records);
    if (kept_records_06 < 4) return 1;
    if (total <= 25) return 2;
    if (records[2].value != 9) return 3;
    return 0;
}
