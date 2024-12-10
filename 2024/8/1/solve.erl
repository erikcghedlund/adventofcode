-module(solve).
-export([main/0]).

-define(NTH(Matrix, X, Y), lists:nth(X, lists:nth(Y, Matrix))).
-define(WIDTH(Matrix), length(lists:nth(1, Matrix))).
-define(HEIGHT(Matrix), length(Matrix)).

% This is not very effecient but I can't be bothered to optimize it
populate_map(Matrix, _, Y, Map) when length(Matrix) < Y -> Map;
populate_map([Row | Matrix], X, Y, Map) when length(Row) < X ->
    populate_map([Row | Matrix], 1, Y + 1, Map);
populate_map(Matrix, X, Y, Map) ->
    case ?NTH(Matrix, X, Y) of
        $. ->
            populate_map(Matrix, X + 1, Y, Map);
        _ ->
            populate_map(
                Matrix,
                X + 1,
                Y,
                maps:put(
                    ?NTH(Matrix, X, Y),
                    [{X, Y} | maps:get(?NTH(Matrix, X, Y), Map, [])],
                    Map
                )
            )
    end.

populate_map(Matrix) -> populate_map(Matrix, 1, 1, maps:new()).

% Neither is this
twocombs(List) ->
    lists:uniq([{lists:min([L1, L2]), lists:max([L1, L2])} || L1 <- List, L2 <- List, L1 /= L2]).

dist({X1, Y1}, {X2, Y2}) -> {X2 - X1, Y2 - Y1}.

get_anti(TwoCombs) ->
    Foo = fun({{X1, Y1}, {X2, Y2}}) ->
        {XDist, YDist} = dist({X1, Y1}, {X2, Y2}),
        [{X1 - XDist, Y1 - YDist}, {X2 + XDist, Y2 + YDist}]
    end,
    lists:flatten(lists:map(Foo, TwoCombs)).

filter_fun({X, Y}, Width, Height) when (X < 1) or (Y < 1) or (Width < X) or (Height < Y) -> false;
filter_fun(_, _, _) -> true.

main() ->
    Matrix = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    Antennnas = populate_map(Matrix),
    TwoCombs = lists:flatmap(fun({_, Val}) -> twocombs(Val) end, maps:to_list(Antennnas)),
    Anti = lists:filter(
        fun(Tup) -> filter_fun(Tup, ?WIDTH(Matrix), ?HEIGHT(Matrix)) end,
        get_anti(TwoCombs)
    ),
    io:format("~p\n", [length(lists:uniq(Anti))]),
    halt().
