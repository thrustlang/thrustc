/* packet_frame.c: literal coverage for protocol header fields. */

static int header_len(void) {
    return 0x10 + 010 + 6;
}

static unsigned int checksum(void) {
    unsigned int c = 0xFFu;
    c ^= 0xA5UL & 0xFFu;
    c += 1u;
    return c;
}

static const char *frame_tag(void) {
    return "PKT" "\x41" "\t";
}

static double latency_ms(int hop) {
    double base = 1.5e1;
    double per = .25;
    if (hop == 0) return 1.;
    return base + hop * per;
}

int packet_frame_check(void) {
    int fail = 0;

    if (header_len() != (0x10 + 8 + 6) && fail == 0) fail = 1;

    unsigned int sum = checksum();
    if (sum != 0x5Bu && fail == 0) fail = 2;

    char soh = '\x01', tab = '\t', esc = '\033';
    char nl = '\n', bs = '\\', qt = '\'';
    if ((soh != 1 || tab != 9 || esc != 27 || nl != 10) && fail == 0) fail = 3;
    if ((bs != 92 || qt != 39) && fail == 0) fail = 4;

    const char *tag = frame_tag();
    const char *msg = "frame:" "\t" "len=0x10\n";
    if ((tag[0] != 'P' || tag[3] != 'A') && fail == 0) fail = 5;
    if ((msg[6] != '\t' || msg[7] != 'l') && fail == 0) fail = 6;

    double t0 = latency_ms(0);
    double t1 = latency_ms(4);
    if ((t0 != 1.0) && fail == 0) fail = 7;
    if ((t1 < 16.0 || t1 > 16.1) && fail == 0) fail = 8;

    long seq = 0x10000L;
    unsigned long total = 0xFFFFFFFFUL;
    if ((seq << 4) != 0x100000L && fail == 0) fail = 9;
    if ((total & 0xFFFFUL) != 0xFFFFUL && fail == 0) fail = 10;

    return fail;
}
