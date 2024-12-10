-module(solve).
-export([main/0]).
-define(XMAX(List),
    case List of
        [
            [$M, _, $M],
            [_, $A, _],
            [$S, _, $S]
        ] ->
            1;
        [
            [$M, _, $S],
            [_, $A, _],
            [$M, _, $S]
        ] ->
            1;
        [
            [$S, _, $M],
            [_, $A, _],
            [$S, _, $M]
        ] ->
            1;
        [
            [$S, _, $S],
            [_, $A, _],
            [$M, _, $M]
        ] ->
            1;
        _ ->
            0
    end
).

find_xmas(Matrix, Row, Column) ->
    ?XMAX([
        lists:sublist(lists:nth(Row - 1, Matrix), Column - 1, 3),
        lists:sublist(lists:nth(Row, Matrix), Column - 1, 3),
        lists:sublist(lists:nth(Row + 1, Matrix), Column - 1, 3)
    ]).

find_xmas(Matrix) ->
    [
        find_xmas(Matrix, Row, Column)
     || Row <- lists:seq(2, length(Matrix) - 1),
        Column <- lists:seq(2, length(lists:nth(1, Matrix)) - 1)
    ].

main() ->
    Lines = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    Count = find_xmas(Lines),
    io:format("~p\n", [lists:sum(Count)]),
    halt().
