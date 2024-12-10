-module(solve).
-export([main/0]).
-define(XMAX(List),
    case List of
        "XMAS" -> 1;
        "SAMX" -> 1;
        _ -> 0
    end
).
-define(NTH(List, Row, Column), lists:nth(Column, lists:nth(Row, List))).

find_horizontal_helper(Index, Line) when Index == (length(Line) - 3) ->
    [?XMAX(lists:sublist(Line, Index, 4))];
find_horizontal_helper(Index, Line) ->
    [?XMAX(lists:sublist(Line, Index, 4)) | find_horizontal_helper(Index + 1, Line)].
find_horizontal_helper(Line) -> find_horizontal_helper(1, Line).

find_horizontal(Matrix) ->
    lists:sum(lists:flatten(lists:map(fun find_horizontal_helper/1, Matrix))).
find_vertical(Matrix) ->
    Transposed = help:transpose(Matrix),
    find_horizontal(Transposed).

find_diagonal_downward(Matrix) ->
    Matches = [
        find_diagonal_downward_help(Matrix, Row, Column)
     || Row <- lists:seq(1, length(Matrix) - 3),
        Column <- lists:seq(1, length(lists:nth(1, Matrix)) - 3)
    ],
    lists:sum(Matches).

find_diagonal_upward(Matrix) ->
    Matches = [
        find_diagonal_upward_help(Matrix, Row, Column)
     || Row <- lists:seq(1, length(Matrix) - 3),
        Column <- lists:seq(4, length(lists:nth(1, Matrix)))
    ],
    lists:sum(Matches).

find_diagonal_downward_help(Matrix, Row, Column) ->
    ?XMAX([
        ?NTH(Matrix, Row, Column),
        ?NTH(Matrix, Row + 1, Column + 1),
        ?NTH(Matrix, Row + 2, Column + 2),
        ?NTH(Matrix, Row + 3, Column + 3)
    ]).

find_diagonal_upward_help(Matrix, Row, Column) ->
    ?XMAX([
        ?NTH(Matrix, Row, Column),
        ?NTH(Matrix, Row + 1, Column - 1),
        ?NTH(Matrix, Row + 2, Column - 2),
        ?NTH(Matrix, Row + 3, Column - 3)
    ]).

main() ->
    Lines = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    Count = [
        find_horizontal(Lines),
        find_vertical(Lines),
        find_diagonal_upward(Lines),
        find_diagonal_downward(Lines)
    ],
    io:format("~p\n", [lists:sum(Count)]),
    halt().
