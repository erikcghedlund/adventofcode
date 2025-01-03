-module(solve).
-export([main/0]).

construct_map(Lines) -> construct_map(Lines, maps:new()).
construct_map([], Accum) ->
    maps:map(fun(_, Val) -> sets:from_list(Val) end, Accum);
construct_map([[C1, C2, _, C3, C4] | Lines], Accum) ->
    Map1 = maps:put([C1, C2], [[C3, C4] | maps:get([C1, C2], Accum, [])], Accum),
    Map2 = maps:put([C3, C4], [[C1, C2] | maps:get([C3, C4], Map1, [])], Map1),
    construct_map(Lines, Map2).

connected(PC1, PC2, Map) ->
    sets:is_element(PC2, maps:get(PC1, Map)).

find_3_cycle_filter([PC1, PC2, PC3], Map) ->
    connected(PC1, PC2, Map) andalso
        connected(PC1, PC3, Map) andalso
        connected(PC2, PC3, Map).

combs(N, List) -> combs(N, List, []).
combs(_, [], Accum) -> Accum;
combs(2, [Head | Tail], Accum) -> combs(2, Tail, [[Head, X] || X <- Tail] ++ Accum);
combs(3, [Head | Tail], Accum) -> combs(3, Tail, [[Head | X] || X <- combs(2, Tail, [])] ++ Accum).

tfun([$t, _]) -> true;
tfun(_) -> false.

three_cycles(Map) ->
    Candidates = lists:filter(
        fun(List) -> lists:any(fun tfun/1, List) end, combs(3, maps:keys(Map))
    ),
    lists:filter(fun(Candidate) -> find_3_cycle_filter(Candidate, Map) end, Candidates).

main() ->
    Lines = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    Map = construct_map(Lines),
    Cycles = three_cycles(Map),
    io:write(length(Cycles)),
    io:nl(),
    halt().
