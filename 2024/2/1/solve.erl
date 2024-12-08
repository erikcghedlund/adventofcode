-module(solve).
-export([main/0]).

is_decreasing([H1, H2]) when H1 > H2 -> true;
is_decreasing([H1, H2 | Tail]) when H1 > H2 -> is_decreasing([H2 | Tail]);
is_decreasing(_) -> false.

is_increasing([H1, H2]) when H1 < H2 -> true;
is_increasing([H1, H2 | Tail]) when H1 < H2 -> is_increasing([H2 | Tail]);
is_increasing(_) -> false.

is_changing_ltthree([H1, H2]) when abs(H1 - H2) =< 3 -> true;
is_changing_ltthree([H1, H2 | Tail]) when abs(H1 - H2) =< 3 -> is_changing_ltthree([H2 | Tail]);
is_changing_ltthree(_) -> false.

is_safe(List) -> (is_increasing(List) or is_decreasing(List)) and is_changing_ltthree(List).

parse(Input) ->
    Lines = string:lexemes(Input, [$\n]),
    IntLines = lists:map(fun (Line) -> lists:map(fun (X) -> help:fst(string:to_integer(X)) end, string:lexemes(Line, [$\s])) end, Lines),
    Safes = lists:filter(fun is_safe/1, IntLines),
    length(Safes).

main() -> 
    io:format("~p\n", [parse(help:stdin())]),
    halt().
