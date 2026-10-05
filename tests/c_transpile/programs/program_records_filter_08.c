#define RECORD_COUNT 7
#define MIN_VALUE 5
struct Record08 { int key; int value; int active; };
const int checksum_bias_08 = 4;
static int kept_records_08 = 0;
static int filter_records_08(struct Record08 *records) {
    int total = 0;
    for (int i = 0; i < RECORD_COUNT; ++i) {
        if (records[i].active && records[i].value >= MIN_VALUE) {
            total += records[i].key + records[i].value + checksum_bias_08;
            kept_records_08 += 1;
        }
    }
    return total;
}
int main(void) {
    struct Record08 records[RECORD_COUNT] = {{1,5,1},{2,7,0},{3,6,1},{4,8,1},{5,4,1},{6,9,1},{7,10,1}};
    int total = filter_records_08(records);
    if (kept_records_08 < 5) return 1;
    if (total <= 35) return 2;
    if (records[0].active != 1) return 3;
    return 0;
}
