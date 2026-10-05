#define RECORD_COUNT 5
#define MIN_VALUE 9
struct Record07 { int key; int value; int active; };
const int checksum_bias_07 = 1;
static int kept_records_07 = 0;
static int filter_records_07(struct Record07 *records) {
    int total = 0;
    for (int i = 0; i < RECORD_COUNT; ++i) {
        if (records[i].active && records[i].value >= MIN_VALUE) {
            total += records[i].key + records[i].value + checksum_bias_07;
            kept_records_07 += 1;
        }
    }
    return total;
}
int main(void) {
    struct Record07 records[RECORD_COUNT] = {{1,9,1},{2,8,1},{3,12,0},{4,10,1},{5,11,1}};
    int total = filter_records_07(records);
    if (kept_records_07 < 3) return 1;
    if (total <= 18) return 2;
    if (records[4].key != 5) return 3;
    return 0;
}
