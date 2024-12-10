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

dfs(_, From, To) when From == To -> true;
dfs(Matrix, {CurX, CurY}, To) ->
    Val = ?NTH(Matrix, CurX, CurY),
    Neigbours = get_neighbours(Matrix, CurX, CurY),
    case lists:filter(fun({_, N}) -> N == Val + 1 end, Neigbours) of
        [] -> false;
        Paths -> lists:any(fun({P, _}) -> dfs(Matrix, P, To) end, Paths)
    end.

allN(Matrix, N) ->
    lists:filter(fun({X, Y}) -> ?NTH(Matrix, X, Y) == N end, [
        {X, Y}
     || X <- lists:seq(1, ?WIDTH(Matrix)), Y <- lists:seq(1, ?HEIGTH(Matrix))
    ]).

main() ->
    Matrix = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    Reduce = lists:map(fun(Row) -> lists:map(fun(X) -> X - $0 end, Row) end, Matrix),
    ArrayMatrix = array:from_list(lists:map(fun array:from_list/1, Reduce)),
    AllTests = [{All0, All9} || All0 <- allN(ArrayMatrix, 0), All9 <- allN(ArrayMatrix, 9)],
    io:format("~p\n", [length(lists:filter(fun({From, To}) -> dfs(ArrayMatrix, From, To) end, AllTests))]),
    halt().
