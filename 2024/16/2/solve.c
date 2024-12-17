#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define NO_OBSTACLES_BUF 10256
#define BEST_BUF 10000
#define MAX(x, y) (x > y) * x + (x <= y) * y
#define ULLMAX ULLONG_MAX
#define HORIZONTAL(dir) ((dir == right) || (dir == left))

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
unsigned long long best_score;
char* best_paths[BEST_BUF];
int best_paths_no;

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
        malloc(max_col * row * 2 * sizeof(unsigned long long));
    for (int i = 0; i < max_col * row * 2; i++) cached_scores[i] = ULLMAX;
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
void draw_map(char* visited) {
    putchar('\n');
    for (int y = 0; y < gamearena.heigth; y++) {
        for (int x = 0; x < gamearena.width; x++) {
            if (gamearena
                    .obstacles[(x % gamearena.width) + y * gamearena.width])
                putchar('#');
            else if (visited[(x % gamearena.width) + y * gamearena.width])
                putchar('O');
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

void set_score(const Cord cord, enum dir dir, unsigned long long score) {
    gamearena
        .cached_scores[(cord.x % gamearena.width) + cord.y * gamearena.width +
                       gamearena.width * gamearena.heigth *
                           (int)HORIZONTAL(dir)] = score;
}

unsigned long long get_score(const Cord cord, enum dir dir) {
    unsigned long long ret =
        gamearena.cached_scores[(cord.x % gamearena.width) +
                                cord.y * gamearena.width +
                                gamearena.width * gamearena.heigth *
                                    (int)HORIZONTAL(dir)];
    return ret;
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
    for (int i = ret; i < 3; i++)
        buf[i] = (CordAndDir){.cord = {.x = -1, .y = -1}, .dir = up};
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

char* create_visited() {
    return calloc(gamearena.heigth * gamearena.width, sizeof(char));
}

void set_visited(char* visited, Cord cord) {
    visited[(cord.x % gamearena.width) + cord.y * gamearena.width] = 1;
}

char* copy_visited(char* visited) {
    char* ret = create_visited();
    memcpy(ret, visited, gamearena.heigth * gamearena.width * sizeof(char));
    return ret;
}

unsigned long long move(Cord pos, enum dir dir, unsigned long long score) {
    if ((pos.x == gamearena.exit.x) && (pos.y == gamearena.exit.y)) {
        return score;
    }
    if (get_score(pos, dir) <= score) {
        return ULLMAX;  // We have already been here with a better score
    }
    set_score(pos, dir, score);
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
    return ULLMAX;
}

void move_with_cache(Cord pos, enum dir dir, unsigned long long score,
                     char* visited) {
    set_visited(visited, pos);
    if ((pos.x == gamearena.exit.x) && (pos.y == gamearena.exit.y)) {
        if (score == best_score)
            best_paths[best_paths_no++] = visited;
        else
            free(visited);
        return;
    }
    if (get_score(pos, dir) < score) {
        free(visited);
        return;
    }
    CordAndDir buf[3];
    switch (get_paths(&pos, dir, buf)) {
        case 0:  // Dead end
            free(visited);
            return;
        case 1:
            move_with_cache(buf[0].cord, buf[0].dir,
                            score + 1 + 1000 * (buf[0].dir != dir), visited);
            break;
        case 2:
            move_with_cache(buf[0].cord, buf[0].dir,
                            score + 1 + 1000 * (buf[0].dir != dir),
                            copy_visited(visited));
            move_with_cache(buf[1].cord, buf[1].dir,
                            score + 1 + 1000 * (buf[1].dir != dir), visited);
            break;
        case 3:
            move_with_cache(buf[0].cord, buf[0].dir,
                            score + 1 + 1000 * (buf[0].dir != dir),
                            copy_visited(visited));
            move_with_cache(buf[1].cord, buf[1].dir,
                            score + 1 + 1000 * (buf[1].dir != dir),
                            copy_visited(visited));
            move_with_cache(buf[2].cord, buf[2].dir,
                            score + 1 + 1000 * (buf[2].dir != dir), visited);
            break;
    }
    return;
}

int unique_paths() {
    char* count = create_visited();
    for (int i = 0; i < best_paths_no; i++) {
        for (int j = 0; j < gamearena.heigth * gamearena.width; j++)
            count[j] |= best_paths[i][j];
        free(best_paths[i]);
    }
    int x = 0;
    for (int j = 0; j < gamearena.heigth * gamearena.width; j++) {
        if (count[j]) x++;
    }
    free(count);
    return x;
}

void destroy_game_arena() {
    free(gamearena.cached_scores);
    free(gamearena.obstacles);
}

int main(void) {
    best_paths_no = 0;
    gamearena = construct_game_arena();
    best_score = move(gamearena.start, right, 0ull);
    move_with_cache(gamearena.start, right, 0ull, create_visited());
    printf("%d\n", unique_paths());
#ifdef DEBUG
    for (int i = 0; i < best_paths_no; i++) draw_map(best_paths[i]);
#endif
    destroy_game_arena();
    return 0;
}
