-module(solve).
-export([main/0]).

map_fun(X) -> help:fst(string:to_integer(X)).

new_map(Map) -> new_map(maps:to_list(Map), maps:new()).
new_map([], Map) ->
    Map;
new_map([{Key, Val} | Tail], Map) ->
    NewMap =
        case blink(Key) of
            [NK1, NK2] ->
                TmpMap = maps:put(NK1, maps:get(NK1, Map, 0) + Val, Map),
                maps:put(NK2, maps:get(NK2, TmpMap, 0) + Val, TmpMap);
            [NK] ->
                maps:put(NK, maps:get(NK, Map, 0) + Val, Map)
        end,
    new_map(Tail, NewMap).

blink_for(N, Stones) ->
    StoneTuple = lists:map(fun (X) -> {X, help:count(X, Stones)} end, Stones),
    blink_for_helper(N, maps:from_list(StoneTuple)).
blink_for_helper(0, Stones) ->
    Stones;
blink_for_helper(N, Stones) ->
    blink_for_helper(N - 1, new_map(Stones)).

blink(0) ->
    [1];
blink(H) ->
    Len = trunc(math:floor(math:log10(H))) + 1,
    if
        (Len rem 2) == 0 ->
            Pow10 = trunc(math:pow(10, Len / 2)),
            [H div Pow10, H rem Pow10];
        true ->
            [H * 2024]
    end.

main() ->
    Stones = lists:map(fun map_fun/1, string:lexemes(lists:flatten(help:stdin()), [$\s])),
    Blinked = blink_for(75, Stones),
    io:format("~p\n", [lists:sum(maps:values(Blinked))]),
    halt().
