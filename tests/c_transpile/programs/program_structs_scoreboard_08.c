#define PLAYER_COUNT 6
#define BONUS_CAP 4
#define FLOOR_SCORE 11

struct Player08 {
    int id;
    int score;
    int bonus;
};

const int base_reward_08 = 2;
static int updates_08 = 0;

static int clamp_bonus_08(int value) {
    if (value < 0) {
        return 0;
    }
    if (value > BONUS_CAP) {
        return BONUS_CAP;
    }
    return value;
}

static void apply_round_08(struct Player08 *player, int delta) {
    player->bonus = clamp_bonus_08(player->bonus + (delta % 2));
    player->score += delta + player->bonus + base_reward_08;
    updates_08 += 1;
}

static int total_score_08(struct Player08 *players) {
    int total = 0;
    for (int i = 0; i < PLAYER_COUNT; ++i) {
        total += players[i].score;
    }
    return total;
}

int main(void) {
    struct Player08 players[PLAYER_COUNT] = {
        {1, 7, 0}, {2, 10, 1}, {3, 12, 2}, {4, 8, 1}, {5, 11, 0}, {6, 9, 3}
    };
    int deltas[PLAYER_COUNT] = {2, 4, 5, 3, 6, 1};
    int best = 0;

    for (int i = 0; i < PLAYER_COUNT; ++i) {
        apply_round_08(&players[i], deltas[i]);
        if (players[i].score > best) {
            best = players[i].score;
        }
    }

    if (updates_08 != PLAYER_COUNT) {
        return 1;
    }
    if (best <= FLOOR_SCORE) {
        return 2;
    }
    if (total_score_08(players) <= 68) {
        return 3;
    }
    return 0;
}
