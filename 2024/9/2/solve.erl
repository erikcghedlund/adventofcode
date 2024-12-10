-module(solve).
-export([main/0]).

-define(ODD(X), (X rem 2) == 1).
-define(TO_INT(C), (C - $0)).

populate_fs([]) ->
    [];
populate_fs([{Index, Int} | Tail]) when ?ODD(Index) ->
    lists:duplicate(Int, empty) ++ populate_fs(Tail);
populate_fs([{Index, Int} | Tail]) ->
    lists:duplicate(Int, (Index div 2)) ++ populate_fs(Tail).

filter_fun({empty, _}) -> false;
filter_fun({X, _}) -> {true, X}.

compress([]) ->
    [];
compress([Head | Tail]) ->
    {Cur, Recurse} = lists:splitwith(fun(X) -> X == Head end, Tail),
    [{Head, length(Cur) + 1} | compress(Recurse)].

uncompress([]) -> [];
uncompress([{Val, Occurences} | Tail]) -> [lists:duplicate(Occurences, Val) | uncompress(Tail)].

move(List) -> move(List, lists:max(lists:filtermap(fun filter_fun/1, List))).
move(List, 0) ->
    List;
move(List, Index) ->
    io:fwrite("Index = ~p\n", [Index]),
    {_, {_, RequiredSpace}} = lists:search(fun({Val, _}) -> Val == Index end, List),
    Candidates = lists:takewhile(fun({X, _}) -> X /= Index end, List),
    IndexIndex = length(Candidates) + 1,
    ICandidates = [
        {I, Occurences}
     || {I, {Val, Occurences}} <- lists:enumerate(Candidates), Val == empty
    ],
    NewList =
        case lists:search(fun({_, Occurences}) -> RequiredSpace =< Occurences end, ICandidates) of
            false ->
                List;
            {value, {I, Occurences}} ->
                {KeepHead, DropTail} = lists:split(I - 1, List),
                {[_ | BeforeIndex], [_ | AfterIndex]} = lists:split(IndexIndex - I, DropTail),
                case Occurences - RequiredSpace of
                    0 ->
                        KeepHead ++ [{Index, RequiredSpace}] ++ BeforeIndex ++
                            [{empty, RequiredSpace}] ++ AfterIndex;
                    N ->
                        KeepHead ++ [{Index, RequiredSpace}] ++ [{empty, N}] ++ BeforeIndex ++
                            [{empty, RequiredSpace}] ++ AfterIndex
                end
        end,
    move(NewList, Index - 1).

checksum_helper(empty, _) -> 0;
checksum_helper(X, Y) -> X * Y.

checksum(List) -> checksum(List, lists:seq(0, length(List) - 1)).
checksum(List, Factors) -> lists:sum(lists:zipwith(fun checksum_helper/2, List, Factors)).

main() ->
    Line = lists:filter(
        fun(X) -> (0 =< X) and (X =< 9) end,
        lists:map(fun(C) -> ?TO_INT(C) end, lists:flatten(help:stdin()))
    ),
    Fs = populate_fs(lists:enumerate(0, Line)),
    Compressed = compress(Fs),
    Moved = move(Compressed),
    Uncompressed = lists:flatten(uncompress(Moved)),
    io:format("~p\n", [checksum(Uncompressed)]),
    halt().
