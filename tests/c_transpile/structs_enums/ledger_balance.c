typedef enum {
    TXN_DEPOSIT = 1,
    TXN_WITHDRAW = -1,
    TXN_FEE = -2,
} TxnKind;
typedef struct {
    int id;
    TxnKind kind;
    int amount;
} Txn;
typedef struct {
    Txn entries[10];
    int count;
    int balance;
} Ledger;

static int txn_delta(Txn t) {
    return (int)t.kind * t.amount;
}
static int ledger_apply(Ledger *ledger, Txn t) {
    if (ledger->count >= 10) return -1;
    ledger->entries[ledger->count] = t;
    ledger->count++;
    ledger->balance += txn_delta(t);
    return 0;
}

static int ledger_count_kind(const Ledger *ledger, TxnKind kind) {
    int n = 0;
    for (int i = 0; i < ledger->count; i++) {
        if (ledger->entries[i].kind == kind) n++;
    }
    return n;
}
static int ledger_largest(const Ledger *ledger) {
    int best = 0;
    for (int i = 0; i < ledger->count; i++) {
        int d = txn_delta(ledger->entries[i]);
        if (d > best) best = d;
    }
    return best;
}

int ledger_balance_check(void) {
    Ledger ledger;
    ledger.count = 0;
    ledger.balance = 0;
    Txn a = { 1, TXN_DEPOSIT, 100 };
    Txn b = { 2, TXN_WITHDRAW, 40 };
    Txn c = { 3, TXN_FEE, 5 };
    if (ledger_apply(&ledger, a) != 0) return 1;
    if (ledger_apply(&ledger, b) != 0) return 2;
    if (ledger_apply(&ledger, c) != 0) return 3;
    if (ledger.balance != 50) return 4;
    if (ledger_count_kind(&ledger, TXN_DEPOSIT) != 1) return 5;
    if (ledger_count_kind(&ledger, TXN_FEE) != 1) return 6;
    if (ledger_largest(&ledger) != 100) return 7;
    if (txn_delta(b) != -40) return 8;
    return 0;
}
