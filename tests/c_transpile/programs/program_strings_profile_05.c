#define BUFFER_SIZE 66
#define MIN_WORD 4
#define BONUS_DIGIT 2
#define BONUS_UPPER 1

const int global_limit_05 = 19;
static int scanned_lines_05 = 0;

static int is_letter_05(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static int is_digit_05(char c) {
    return c >= '0' && c <= '9';
}

static int count_words_05(const char *text) {
    int words = 0;
    int length = 0;
    for (int i = 0; text[i] != '\0'; ++i) {
        if (is_letter_05(text[i])) {
            length += 1;
        } else {
            if (length >= MIN_WORD) {
                words += 1;
            }
            length = 0;
        }
    }
    if (length >= MIN_WORD) {
        words += 1;
    }
    return words;
}

static int profile_line_05(const char *text) {
    int score = 0;
    for (int i = 0; text[i] != '\0' && i < BUFFER_SIZE; ++i) {
        if (is_digit_05(text[i])) {
            score += BONUS_DIGIT;
        } else if (text[i] >= 'A' && text[i] <= 'Z') {
            score += BONUS_UPPER;
        } else if (text[i] == '.') {
            score += 1;
        }
    }
    scanned_lines_05 += 1;
    return score + count_words_05(text);
}

int main(void) {
    const char *items[4] = {
        "node.31 alpha field",
        "TRACE beam 222",
        "inner path token_7",
        "FINAL.LOG 600"
    };
    int total = 0;

    for (int i = 0; i < 4; ++i) {
        total += profile_line_05(items[i]);
    }

    if (scanned_lines_05 != 4) {
        return 1;
    }
    if (total <= global_limit_05) {
        return 2;
    }
    if (count_words_05(items[1]) < 1) {
        return 3;
    }
    return 0;
}
