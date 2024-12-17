#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define NO_OBSTACLES_BUF 10256
#define MAX(x, y) (x > y) * x + (x <= y) * y
#define ULLMAX ULLONG_MAX

typedef struct Cord {
    int x, y;
} Cord;

typedef struct GameArena {
    int width, heigth;
    struct Cord start, exit;
    char* obstacles;
    unsigned long long* cached_scores;
} GameArena;

enum dir { up, down, left, right };

typedef struct CordAndDir {
    struct Cord cord;
    enum dir dir;
} CordAndDir;

unsigned long long min(unsigned long long x, unsigned long long y) {
    if (x < y) return x;
    return y;
}

GameArena gamearena;

GameArena construct_game_arena() {
    Cord obstacles_buf[NO_OBSTACLES_BUF];
    Cord start, exit;
    int no_obstacles = 0;
    int col = 0, row = 0, max_col = 0;
    for (int scan_more = 1; scan_more; col++) {
        switch (getchar()) {
            case '#':
                obstacles_buf[no_obstacles++] = (Cord){.x = col, .y = row};
                break;
            case '\n':
                max_col = MAX(max_col, col);
                col = -1;
                row++;
                break;
            case 'S':
                start = (Cord){.x = col, .y = row};
                break;
            case 'E':
                exit = (Cord){.x = col, .y = row};
                break;
            case EOF:
                scan_more = 0;
                break;
        }
    }
    unsigned long long* cached_scores =
        calloc(max_col * row, sizeof(unsigned long long));
    for (int i = 0; i < max_col * row; i++) cached_scores[i] = ULLMAX;
    char* obstacles = calloc(max_col * row, sizeof(char));
    for (int i = 0; i < no_obstacles; i++)
        obstacles[(obstacles_buf[i].x % max_col) +
                  obstacles_buf[i].y * max_col] = 1;
    return (GameArena){.width = max_col,
                       .heigth = row,
                       .start = start,
                       .exit = exit,
                       .obstacles = obstacles,
                       .cached_scores = cached_scores};
}

#ifdef DEBUG
void draw_map() {
    for (int y = 0; y < gamearena.heigth; y++) {
        for (int x = 0; x < gamearena.width; x++) {
            if (gamearena
                    .obstacles[(x % gamearena.width) + y * gamearena.width])
                putchar('#');
            else
                putchar('.');
        }
        putchar('\n');
    }
    return;
}
#endif

int has_obstacle(const Cord cord) {
    return (gamearena.obstacles[(cord.x % gamearena.width) +
                                cord.y * gamearena.width]);
}

void set_score(const Cord cord, unsigned long long score) {
    gamearena
        .cached_scores[(cord.x % gamearena.width) + cord.y * gamearena.width] =
        score;
}

unsigned long long get_score(const Cord cord) {
    return gamearena
        .cached_scores[(cord.x % gamearena.width) + cord.y * gamearena.width];
}

Cord add_dir(const Cord* pos, enum dir dir) {
    Cord ret = *pos;
    switch (dir) {
        case up:
            ret.y--;
            break;
        case down:
            ret.y++;
            break;
        case left:
            ret.x--;
            break;
        case right:
            ret.x++;
            break;
    }
    return ret;
}

int get_paths(const Cord* pos, enum dir dir, CordAndDir* buf) {
    int ret = 0;
    switch (dir) {
        case up:
            if (!has_obstacle(add_dir(pos, up)))
                buf[ret++] = (CordAndDir){.cord = add_dir(pos, up), .dir = up};
            if (!has_obstacle(add_dir(pos, left)))
                buf[ret++] =
                    (CordAndDir){.cord = add_dir(pos, left), .dir = left};
            if (!has_obstacle(add_dir(pos, right)))
                buf[ret++] =
                    (CordAndDir){.cord = add_dir(pos, right), .dir = right};
            break;
        case down:
            if (!has_obstacle(add_dir(pos, down)))
                buf[ret++] =
                    (CordAndDir){.cord = add_dir(pos, down), .dir = down};
            if (!has_obstacle(add_dir(pos, left)))
                buf[ret++] =
                    (CordAndDir){.cord = add_dir(pos, left), .dir = left};
            if (!has_obstacle(add_dir(pos, right)))
                buf[ret++] =
                    (CordAndDir){.cord = add_dir(pos, right), .dir = right};
            break;
        case right:
            if (!has_obstacle(add_dir(pos, right)))
                buf[ret++] =
                    (CordAndDir){.cord = add_dir(pos, right), .dir = right};
            if (!has_obstacle(add_dir(pos, down)))
                buf[ret++] =
                    (CordAndDir){.cord = add_dir(pos, down), .dir = down};
            if (!has_obstacle(add_dir(pos, up)))
                buf[ret++] = (CordAndDir){.cord = add_dir(pos, up), .dir = up};
            break;
        case left:
            if (!has_obstacle(add_dir(pos, left)))
                buf[ret++] =
                    (CordAndDir){.cord = add_dir(pos, left), .dir = left};
            if (!has_obstacle(add_dir(pos, down)))
                buf[ret++] =
                    (CordAndDir){.cord = add_dir(pos, down), .dir = down};
            if (!has_obstacle(add_dir(pos, up)))
                buf[ret++] = (CordAndDir){.cord = add_dir(pos, up), .dir = up};
            break;
    }
    return ret;
}

void dir_to_str(enum dir dir, char* buf) {
    switch (dir) {
        case up:
            strncpy(buf, "up", 2);
            break;
        case down:
            strncpy(buf, "down", 4);
            break;
        case left:
            strncpy(buf, "left", 4);
            break;
        case right:
            strncpy(buf, "right", 5);
            break;
    }
}

unsigned long long move(Cord pos, enum dir dir, unsigned long long score) {
    char str[6];
    dir_to_str(dir, str);
    if ((pos.x == gamearena.exit.x) && (pos.y == gamearena.exit.y)) {
        return score;
    }
    if (get_score(pos) <= score) {
        return ULLMAX;  // We have already been here with a better score
    }
    set_score(pos, score);
    CordAndDir buf[3];
    switch (get_paths(&pos, dir, buf)) {
        case 0:  // Dead end
            return ULLMAX;
        case 1:
            return move(buf[0].cord, buf[0].dir,
                        score + 1 + 1000 * (buf[0].dir != dir));
        case 2:
            return min(move(buf[0].cord, buf[0].dir,
                            score + 1 + 1000 * (buf[0].dir != dir)),
                       move(buf[1].cord, buf[1].dir,
                            score + 1 + 1000 * (buf[1].dir != dir)));
        case 3:
            return min(min(move(buf[0].cord, buf[0].dir,
                                score + 1 + 1000 * (buf[0].dir != dir)),
                           move(buf[1].cord, buf[1].dir,
                                score + 1 + 1000 * (buf[1].dir != dir))),
                       move(buf[2].cord, buf[2].dir,
                            score + 1 + 1000 * (buf[2].dir != dir)));
    }
    puts("Panic!");
    return ULLMAX;  // We have already been here with a better score
}

void destroy_game_arena() {
    free(gamearena.cached_scores);
    free(gamearena.obstacles);
}

int main(void) {
    gamearena = construct_game_arena();
#ifdef DEBUG
    draw_map();
#endif
    printf("%llu\n", move(gamearena.start, right, 0ull));
    destroy_game_arena();
    return 0;
}
