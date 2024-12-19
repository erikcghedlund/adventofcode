-module(solve).
-export([main/0]).
-define(MAGIC_NUMBER, 1000000000000000000000).
-ifdef(TEST).
-define(DIMENSION, 6).
-define(READ, 12).
-else.
-define(DIMENSION, 70).
-define(READ, 1024).
-endif.

get_obstacles(Lines) -> get_obstacles(Lines, []).
get_obstacles([], Accum) ->
    BorderUp = [{X, -1} || X <- lists:seq(0, 70)],
    BorderDown = [{X, ?DIMENSION + 1} || X <- lists:seq(0, 70)],
    BorderLeft = [{-1, Y} || Y <- lists:seq(0, 70)],
    BorderRight = [{?DIMENSION + 1, Y} || Y <- lists:seq(0, 70)],
    LimitedAccum = lists:sublist(lists:reverse(Accum), ?READ),
    sets:from_list(LimitedAccum ++ BorderUp ++ BorderDown ++ BorderLeft ++ BorderRight);
get_obstacles([Line | Lines], Accum) ->
    {X, [_ | Rest]} = string:to_integer(Line),
    {Y, _} = string:to_integer(Rest),
    get_obstacles(Lines, [{X, Y} | Accum]).

get_paths({X, Y}, Dir, Obstacles) ->
    Posibilities = [{right, {X + 1, Y}}, {left, {X - 1, Y}}, {down, {X, Y + 1}}, {up, {X, Y - 1}}],
    % We never turn 180 degrees or go into an obstacle
    Forbidden = sets:add_element(
        case Dir of
            left -> {X + 1, Y};
            right -> {X - 1, Y};
            up -> {X, Y + 1};
            down -> {X, Y - 1}
        end,
        Obstacles
    ),
    lists:filter(fun({_, Elem}) -> not sets:is_element(Elem, Forbidden) end, Posibilities).

dfs(_, [], CachedScores) ->
    maps:get({?DIMENSION, ?DIMENSION}, CachedScores);
dfs(Obstacles, [{{Dir, Position}, Score} | Stack], CachedScores) ->
    case maps:get(Position, CachedScores, ?MAGIC_NUMBER) =< Score of
        % We have already been here in a better case, just move on...
        true ->
            dfs(Obstacles, Stack, CachedScores);
        false ->
            NewMap = maps:put(Position, Score, CachedScores),
            Paths = get_paths(Position, Dir, Obstacles),
            Zipped = lists:zip(Paths, lists:duplicate(3, Score + 1), trim),
            dfs(Obstacles, Zipped ++ Stack, NewMap)
    end.

solve(Obstacles) -> dfs(Obstacles, [{{right, {0, 0}}, 0}], maps:new()).

main() ->
    Lines = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    Obstacles = get_obstacles(Lines),
    io:format("~p\n", [solve(Obstacles)]),
    halt().
