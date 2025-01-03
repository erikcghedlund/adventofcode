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

all_connected([PC1, PC2], Map) ->
    connected(PC1, PC2, Map);
all_connected([Head | Tail], Map) ->
    case lists:all(fun(PC) -> connected(Head, PC, Map) end, Tail) of
        true -> all_connected(Tail, Map);
        false -> false
    end.

combs(N, List) -> combs(N, List, []).
combs(_, [], Accum) ->
    Accum;
combs(2, [Head | Tail], Accum) ->
    combs(2, Tail, [[Head, X] || X <- Tail] ++ Accum);
combs(N, [Head | Tail], Accum) ->
    combs(N, Tail, [[Head | X] || X <- combs(N - 1, Tail, [])] ++ Accum).

solve(Map) -> solve(Map, lists:max(lists:map(fun sets:size/1, maps:values(Map)))).
solve(Map, N) ->
    io:write(N),
    io:nl(),
    case
        lists:search(
            fun(X) -> X /= false end,
            lists:map(
                fun({Key, Val}) -> solve_helper(Map, N, [Key | sets:to_list(Val)]) end,
                maps:to_list(Map)
            )
        )
    of
        {value, Comb} -> lists:join($,, lists:sort(Comb));
        false -> solve(Map, N - 1)
    end.
solve_helper(Map, N, List) ->
    case lists:search(fun(Comb) -> all_connected(Comb, Map) end, combs(N, List)) of
        {value, Comb} -> Comb;
        false -> false
    end.

main() ->
    Lines = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    Map = construct_map(Lines),
    io:fwrite(solve(Map)),
    io:nl(),
    halt().
