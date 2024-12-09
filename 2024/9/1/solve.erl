-module(solve).
-export([main/0]).

-define(ODD(X), (X rem 2) == 1).
-define(TO_INT(C), (C - $0)).

populate_fs([]) -> [];
populate_fs([{Index, Int} | Tail]) when ?ODD(Index) -> lists:duplicate(Int, empty) ++ populate_fs(Tail);
populate_fs([{Index, Int} | Tail]) -> lists:duplicate(Int, (Index div 2)) ++ populate_fs(Tail).

isempty(empty) -> true;
isempty(_) -> false.
notempty(X) -> not isempty(X).

move(List) -> lists:filter(fun notempty/1, move(List, [])).
move(List, Accum) -> case not lists:suffix(lists:filter(fun isempty/1, List), List) of
                         true ->
                            {ToKeep, ToWork} = lists:splitwith(fun notempty/1, List),
                            DropLen = length(lists:takewhile(fun isempty/1, ToWork)),
                            ToAppend = lists:sublist(lists:reverse(lists:filter(fun notempty/1, List)), DropLen),
                            ToRecurse = lists:reverse(lists:subtract(lists:reverse(lists:nthtail(DropLen, ToWork)), ToAppend)) ++ lists:duplicate(DropLen, empty),
                            move(ToRecurse, Accum ++ ToKeep ++ ToAppend);
                         false ->
                             Accum ++ List
                     end.

checksum(List) -> checksum(List, lists:seq(0, length(List) - 1)).
checksum(List, Factors) -> lists:sum(lists:zipwith(fun (X, Y) -> X*Y end, List, Factors)).

main() ->
    Line = lists:filter(fun (X) -> (0 =< X) and (X =< 9) end, lists:map(fun (C) -> ?TO_INT(C) end, lists:flatten(help:stdin()))),
    io:write(Line), io:nl(),
    Fs = populate_fs(lists:enumerate(0, Line)),
    io:write(Fs), io:nl(),
    Moved = move(Fs),
    io:write(Moved), io:nl(),
    io:format("~p\n", [checksum(Moved)]),
    halt().
