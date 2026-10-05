#define RECORD_COUNT 6
#define MIN_VALUE 8
struct Record09 { int key; int value; int active; };
const int checksum_bias_09 = 2;
static int kept_records_09 = 0;
static int filter_records_09(struct Record09 *records) {
    int total = 0;
    for (int i = 0; i < RECORD_COUNT; ++i) {
        if (records[i].active && records[i].value >= MIN_VALUE) {
            total += records[i].key + records[i].value + checksum_bias_09;
            kept_records_09 += 1;
        }
    }
    return total;
}
int main(void) {
    struct Record09 records[RECORD_COUNT] = {{1,10,1},{2,8,1},{3,6,0},{4,9,1},{5,12,1},{6,7,1}};
    int total = filter_records_09(records);
    if (kept_records_09 < 4) return 1;
    if (total <= 28) return 2;
    if (records[3].value != 9) return 3;
    return 0;
}
