#define BUFFER_SIZE 80
#define MIN_WORD 5
#define BONUS_DIGIT 1
#define BONUS_UPPER 3

const int global_limit_04 = 15;
static int scanned_lines_04 = 0;

static int is_letter_04(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static int is_digit_04(char c) {
    return c >= '0' && c <= '9';
}

static int count_words_04(const char *text) {
    int words = 0;
    int length = 0;
    for (int i = 0; text[i] != '\0'; ++i) {
        if (is_letter_04(text[i])) {
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

static int profile_line_04(const char *text) {
    int score = 0;
    for (int i = 0; text[i] != '\0' && i < BUFFER_SIZE; ++i) {
        if (is_digit_04(text[i])) {
            score += BONUS_DIGIT;
        } else if (text[i] >= 'A' && text[i] <= 'Z') {
            score += BONUS_UPPER;
        } else if (text[i] == '/') {
            score += 1;
        }
    }
    scanned_lines_04 += 1;
    return score + count_words_04(text);
}

int main(void) {
    const char *items[4] = {
        "PATH/99 vector field",
        "OMEGA relay 123",
        "delta route marker_5",
        "STACK/MODE 204"
    };
    int total = 0;

    for (int i = 0; i < 4; ++i) {
        total += profile_line_04(items[i]);
    }

    if (scanned_lines_04 != 4) {
        return 1;
    }
    if (total <= global_limit_04) {
        return 2;
    }
    if (count_words_04(items[0]) < 2) {
        return 3;
    }
    return 0;
}
