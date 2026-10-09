typedef enum {
    PKT_ACK  = 0x1,
    PKT_DATA = 0x2,
    PKT_NAK  = 0x4,
} PktFlags;
typedef struct {
    int word;
    unsigned char bytes[4];
} Payload;
typedef struct {
    int seq;
    PktFlags flags;
    Payload payload;
} Packet;
typedef struct {
    Packet slots[6];
    int head;
    int tail;
} Queue;

static unsigned char payload_byte(Payload p, int index) {
    if (index < 0 || index > 3) return 0;
    return p.bytes[index];
}
static int packet_has(Packet p, PktFlags flag) {
    return (p.flags & flag) != 0;
}
static int queue_push(Queue *q, Packet p) {
    int next = (q->tail + 1) % 6;
    if (next == q->head) return -1;
    q->slots[q->tail] = p;
    q->tail = next;
    return 0;
}
static int queue_count_flags(const Queue *q, PktFlags flag) {
    int n = 0;
    int i = q->head;
    while (i != q->tail) {
        if (packet_has(q->slots[i], flag)) n++;
        i = (i + 1) % 6;
    }
    return n;
}

int packet_queue_check(void) {
    Queue q = { .head = 0, .tail = 0 };
    Packet a = { .seq = 1, .flags = PKT_DATA, .payload = { .word = 0x01020304, .bytes = { 0x04, 0x03, 0x02, 0x01 } } };
    Packet b = { .seq = 2, .flags = (PktFlags)(PKT_ACK | PKT_NAK), .payload = { .word = 0, .bytes = { 0, 0, 0, 0 } } };
    if (queue_push(&q, a) != 0) return 1;
    if (queue_push(&q, b) != 0) return 2;
    if (q.tail != 2) return 3;
    if (queue_count_flags(&q, PKT_ACK) != 1) return 4;
    if (queue_count_flags(&q, PKT_DATA) != 1) return 5;
    if (payload_byte(a.payload, 0) != (unsigned char)0x04) return 6;
    if (payload_byte(b.payload, 3) != 0) return 7;
    if (!packet_has(a, PKT_DATA)) return 8;
    return 0;
}
