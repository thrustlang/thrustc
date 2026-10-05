#define PLAYER_COUNT 6
#define BONUS_CAP 4
#define FLOOR_SCORE 11

struct Player04 {
    int id;
    int score;
    int bonus;
};

const int base_reward_04 = 3;
static int updates_04 = 0;

static int clamp_bonus_04(int value) {
    if (value < 0) {
        return 0;
    }
    if (value > BONUS_CAP) {
        return BONUS_CAP;
    }
    return value;
}

static void apply_round_04(struct Player04 *player, int delta) {
    player->bonus = clamp_bonus_04(player->bonus + (delta % 2));
    player->score += delta + player->bonus + base_reward_04;
    updates_04 += 1;
}

static int total_score_04(struct Player04 *players) {
    int total = 0;
    for (int i = 0; i < PLAYER_COUNT; ++i) {
        total += players[i].score;
    }
    return total;
}

int main(void) {
    struct Player04 players[PLAYER_COUNT] = {
        {1, 8, 1}, {2, 12, 1}, {3, 9, 0}, {4, 10, 2}, {5, 7, 3}, {6, 11, 1}
    };
    int deltas[PLAYER_COUNT] = {2, 4, 3, 5, 1, 6};
    int best = 0;

    for (int i = 0; i < PLAYER_COUNT; ++i) {
        apply_round_04(&players[i], deltas[i]);
        if (players[i].score > best) {
            best = players[i].score;
        }
    }

    if (updates_04 != PLAYER_COUNT) {
        return 1;
    }
    if (best <= FLOOR_SCORE) {
        return 2;
    }
    if (total_score_04(players) <= 72) {
        return 3;
    }
    return 0;
}
