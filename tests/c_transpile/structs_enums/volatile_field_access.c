struct Box {
    volatile int value;
    int other;
};

int field_read(struct Box *box) {
    return box->value;
}

int field_write(struct Box *box, int value) {
    box->value = value;
    return box->value;
}
