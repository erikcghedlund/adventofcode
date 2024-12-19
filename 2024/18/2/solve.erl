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
    BorderUp = [{X, -1} || X <- lists:seq(0, ?DIMENSION)],
    BorderDown = [{X, ?DIMENSION + 1} || X <- lists:seq(0, ?DIMENSION)],
    BorderLeft = [{-1, Y} || Y <- lists:seq(0, ?DIMENSION)],
    BorderRight = [{?DIMENSION + 1, Y} || Y <- lists:seq(0, ?DIMENSION)],
    BorderUp ++ BorderDown ++ BorderLeft ++ BorderRight ++ lists:reverse(Accum);
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
    CachedScores;
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

neighbour({Ax, Ay}, {Bx, By}) ->
    ((abs(Ax - Bx) == 1) and (Ay == By)) or
        ((abs(Ay - By) == 1) and (Ax == Bx)).

reconstruct_path(CachedScores) -> reconstruct_path(CachedScores, {?DIMENSION, ?DIMENSION}, []).
reconstruct_path(_, {0, 0}, Accum) ->
    [{0, 0} | Accum];
reconstruct_path(CachedScores, Position, Accum) ->
    Score = maps:get(Position, CachedScores),
    {value, {Previous, _}} = lists:search(
        fun({CandidatePos, CandidateScore}) ->
            CandidateScore == (Score - 1) andalso neighbour(Position, CandidatePos)
        end,
        maps:to_list(CachedScores)
    ),
    reconstruct_path(CachedScores, Previous, [Position | Accum]).

solve(OldObstacles, [Candidate | NewObstacles], Rest) ->
    ObstacleSet = sets:union(OldObstacles, sets:from_list([Candidate | NewObstacles])),
    CachedScores = dfs(ObstacleSet, [{{right, {0, 0}}, 0}], maps:new()),
    case maps:is_key({?DIMENSION, ?DIMENSION}, CachedScores) of
        false ->
            Candidate;
        true ->
            PathTaken = sets:from_list(reconstruct_path(CachedScores)),
            {NewCandidates, [Dept | RestRest]} = lists:splitwith(
                fun(O) -> not sets:is_element(O, PathTaken) end, Rest
            ),
            solve(ObstacleSet, [Dept | NewCandidates], RestRest)
    end.

solve(Obstacles) ->
    {FirstObstacle, Rest} = lists:split(?DIMENSION * 4 + ?READ, Obstacles),
    ObstacleSet = sets:from_list(FirstObstacle),
    CachedScores = dfs(ObstacleSet, [{{right, {0, 0}}, 0}], maps:new()),
    PathTaken = sets:from_list(reconstruct_path(CachedScores)),
    {NewCandidates, [Dept | RestRest]} = lists:splitwith(
        fun(O) -> not sets:is_element(O, PathTaken) end, Rest
    ),
    solve(ObstacleSet, [Dept | NewCandidates], RestRest).

main() ->
    Lines = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    Obstacles = get_obstacles(Lines),
    {SolutionX, SolutionY} = solve(Obstacles),
    io:format("~p,~p\n", [SolutionX, SolutionY]),
    halt().
