#define BUFFER_SIZE 76
#define MIN_WORD 3
#define BONUS_DIGIT 2
#define BONUS_UPPER 1

const int global_limit_09 = 21;
static int scanned_lines_09 = 0;

static int is_letter_09(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static int is_digit_09(char c) {
    return c >= '0' && c <= '9';
}

static int count_words_09(const char *text) {
    int words = 0;
    int length = 0;
    for (int i = 0; text[i] != '\0'; ++i) {
        if (is_letter_09(text[i])) {
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

static int profile_line_09(const char *text) {
    int score = 0;
    for (int i = 0; text[i] != '\0' && i < BUFFER_SIZE; ++i) {
        if (is_digit_09(text[i])) {
            score += BONUS_DIGIT;
        } else if (text[i] >= 'A' && text[i] <= 'Z') {
            score += BONUS_UPPER;
        } else if (text[i] == '@') {
            score += 1;
        }
    }
    scanned_lines_09 += 1;
    return score + count_words_09(text);
}

int main(void) {
    const char *items[4] = {
        "cache@31 alpha node",
        "UPPER zone 345",
        "tiny route field_1",
        "FINAL@LINK 707"
    };
    int total = 0;

    for (int i = 0; i < 4; ++i) {
        total += profile_line_09(items[i]);
    }

    if (scanned_lines_09 != 4) {
        return 1;
    }
    if (total <= global_limit_09) {
        return 2;
    }
    if (count_words_09(items[3]) < 1) {
        return 3;
    }
    return 0;
}
