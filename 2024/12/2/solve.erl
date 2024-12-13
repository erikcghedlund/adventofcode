-module(solve).
-export([main/0]).

-define(NTH(Matrix, X, Y), array:get(X - 1, array:get(Y - 1, Matrix))).
-define(WIDTH(Matrix), array:size(array:get(0, Matrix))).
-define(HEIGTH(Matrix), array:size(Matrix)).

filter_fun(Matrix, XPos, YPos) ->
    filter_fun(Matrix, XPos, YPos, ?WIDTH(Matrix) + 1, ?HEIGTH(Matrix) + 1).
filter_fun(Matrix, XPos, YPos, Width, Height) ->
    case {XPos, YPos} of
        {0, _} -> false;
        {_, 0} -> false;
        {Width, _} -> false;
        {_, Height} -> false;
        _ -> {true, {{XPos, YPos}, ?NTH(Matrix, XPos, YPos)}}
    end.

get_neighbours(Matrix, XPos, YPos) ->
    lists:filtermap(fun({X, Y}) -> filter_fun(Matrix, X, Y) end, [
        {XPos + 1, YPos},
        {XPos - 1, YPos},
        {XPos, YPos + 1},
        {XPos, YPos - 1}
    ]).

dfs(Matrix, From) -> lists:uniq(dfs(Matrix, [From], sets:new())).

dfs(_, [], Visited) ->
    sets:to_list(Visited);
dfs(Matrix, [{CurX, CurY} | Tail], Visited) ->
    case sets:is_element({CurX, CurY}, Visited) of
        true ->
            dfs(Matrix, Tail, Visited);
        false ->
            NewSet = sets:add_element({CurX, CurY}, Visited),
            Val = ?NTH(Matrix, CurX, CurY),
            Neigbours = get_neighbours(Matrix, CurX, CurY),
            Filtered = lists:map(
                fun help:fst/1, lists:filter(fun({_, N}) -> N == Val end, Neigbours)
            ),
            dfs(Matrix, Filtered ++ Tail, NewSet)
    end.

construct_map(Matrix) ->
    construct_map(
        Matrix,
        lists:reverse([
            {X, Y}
         || X <- lists:seq(1, ?WIDTH(Matrix)), Y <- lists:seq(1, ?HEIGTH(Matrix))
        ]),
        maps:new()
    ).

construct_map(_, [], Map) ->
    Map;
construct_map(Matrix, [{XPos, YPos} | Tail], Map) ->
    Letter = ?NTH(Matrix, XPos, YPos),
    NewSet = [{XPos, YPos} | maps:get(Letter, Map, [])],
    NewMap = maps:put(Letter, NewSet, Map),
    construct_map(Matrix, Tail, NewMap).

divide_areas(Matrix, Map) ->
    maps:map(fun(_, Set) -> divide_areas(Matrix, Set, []) end, Map).

divide_areas(_, [], NewSet) ->
    NewSet;
divide_areas(Matrix, [Pos | Tail], NewSet) ->
    Area = dfs(Matrix, Pos),
    divide_areas(Matrix, lists:subtract(Tail, Area), [Area | NewSet]).

discount(Walls, Dir, {Angle, {XPos, YPos}}) ->
    discount(Walls, Dir, {Angle, {XPos, YPos}}, []).

discount(Walls, Dir, {Angle, {XPos, YPos}}, Accum) ->
    NewPos =
        case Dir of
            up -> {XPos, YPos + 1};
            down -> {XPos, YPos - 1};
            left -> {XPos - 1, YPos};
            right -> {XPos + 1, YPos}
        end,
    case lists:member({Angle, NewPos}, Walls) of
        true -> discount(Walls, Dir, {Angle, NewPos}, [{Angle, {XPos, YPos}} | Accum]);
        false -> [{Angle, {XPos, YPos}} | Accum]
    end.

discount([], Accum) ->
    Accum;
discount([{Angle, Head} | Wall], Accum) ->
    Dirs =
        case Angle of
            leftmost -> [up, down];
            rightmost -> [up, down];
            downward -> [left, right];
            upward -> [left, right]
        end,
    Rm = lists:flatmap(fun(Dir) -> discount(Wall, Dir, {Angle, Head}) end, Dirs),
    NewWall = lists:subtract(Wall, Rm),
    discount(NewWall, [[{Angle, Head} | Rm] | Accum]).

% This is also quite inefficent but I hope and pray it is fast enough
get_perimeter_of_area(Area) ->
    AllAdjacents = lists:flatmap(
        fun({XPos, YPos}) ->
            [
                {leftmost, {XPos + 1, YPos}},
                {rightmost, {XPos - 1, YPos}},
                {downward, {XPos, YPos + 1}},
                {upward, {XPos, YPos - 1}}
            ]
        end,
        Area
    ),
    AreaSet = sets:from_list(Area),
    Perimeter = lists:filter(fun({_, X}) -> not sets:is_element(X, AreaSet) end, AllAdjacents),
    Discounted = discount(Perimeter, []),
    Discounted.

deep_helper(List) -> lists:map(fun(L) -> length(L) * length(get_perimeter_of_area(L)) end, List).

main() ->
    Matrix = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    ArrayMatrix = array:from_list(lists:map(fun array:from_list/1, Matrix)),
    CropMap = construct_map(ArrayMatrix),
    AreaMap = divide_areas(ArrayMatrix, CropMap),
    Values = lists:flatmap(fun deep_helper/1, maps:values(AreaMap)),
    io:format("~p\n", [lists:sum(Values)]),
    halt().
