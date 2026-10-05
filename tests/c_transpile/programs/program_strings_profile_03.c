#define BUFFER_SIZE 68
#define MIN_WORD 3
#define BONUS_DIGIT 2
#define BONUS_UPPER 2

const int global_limit_03 = 20;
static int scanned_lines_03 = 0;

static int is_letter_03(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static int is_digit_03(char c) {
    return c >= '0' && c <= '9';
}

static int count_words_03(const char *text) {
    int words = 0;
    int length = 0;
    for (int i = 0; text[i] != '\0'; ++i) {
        if (is_letter_03(text[i])) {
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

static int profile_line_03(const char *text) {
    int score = 0;
    for (int i = 0; text[i] != '\0' && i < BUFFER_SIZE; ++i) {
        if (is_digit_03(text[i])) {
            score += BONUS_DIGIT;
        } else if (text[i] >= 'A' && text[i] <= 'Z') {
            score += BONUS_UPPER;
        } else if (text[i] == ':') {
            score += 1;
        }
    }
    scanned_lines_03 += 1;
    return score + count_words_03(text);
}

int main(void) {
    const char *items[4] = {
        "CORE:44 alpha path",
        "beta TRACE 731",
        "index map cell_2",
        "FINAL:STACK 9000"
    };
    int total = 0;

    for (int i = 0; i < 4; ++i) {
        total += profile_line_03(items[i]);
    }

    if (scanned_lines_03 != 4) {
        return 1;
    }
    if (total <= global_limit_03) {
        return 2;
    }
    if (count_words_03(items[2]) < 2) {
        return 3;
    }
    return 0;
}
