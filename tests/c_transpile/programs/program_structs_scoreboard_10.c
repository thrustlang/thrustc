#define PLAYER_COUNT 6
#define BONUS_CAP 5
#define FLOOR_SCORE 12

struct Player10 {
    int id;
    int score;
    int bonus;
};

const int base_reward_10 = 3;
static int updates_10 = 0;

static int clamp_bonus_10(int value) {
    if (value < 0) {
        return 0;
    }
    if (value > BONUS_CAP) {
        return BONUS_CAP;
    }
    return value;
}

static void apply_round_10(struct Player10 *player, int delta) {
    player->bonus = clamp_bonus_10(player->bonus + (delta % 3));
    player->score += delta + player->bonus + base_reward_10;
    updates_10 += 1;
}

static int total_score_10(struct Player10 *players) {
    int total = 0;
    for (int i = 0; i < PLAYER_COUNT; ++i) {
        total += players[i].score;
    }
    return total;
}

int main(void) {
    struct Player10 players[PLAYER_COUNT] = {
        {1, 9, 1}, {2, 8, 2}, {3, 12, 1}, {4, 11, 0}, {5, 10, 3}, {6, 7, 1}
    };
    int deltas[PLAYER_COUNT] = {4, 2, 5, 3, 6, 1};
    int best = 0;

    for (int i = 0; i < PLAYER_COUNT; ++i) {
        apply_round_10(&players[i], deltas[i]);
        if (players[i].score > best) {
            best = players[i].score;
        }
    }

    if (updates_10 != PLAYER_COUNT) {
        return 1;
    }
    if (best <= FLOOR_SCORE) {
        return 2;
    }
    if (total_score_10(players) <= 73) {
        return 3;
    }
    return 0;
}
