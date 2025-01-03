-module(solve).
-export([main/0]).
-define(MODNUM, 16777216).

maximum_fun({_, Val1}, {_, Val2}) -> Val2 < Val1.
maximum_by(Fun, List) -> lists:nth(1, lists:sort(Fun, List)).

enumerate(List) -> lists:zip(lists:seq(1, length(List)), List).

mix(Secret, Val) -> Secret bxor Val.

prune(Secret) -> Secret rem ?MODNUM.

step(1, Secret) -> prune(mix(Secret, Secret * 64));
step(2, Secret) -> prune(mix(Secret, Secret div 32));
step(3, Secret) -> prune(mix(Secret, Secret * 2048)).

next_random(Secret) -> lists:foldl(fun (I, Acc) -> step(I, Acc) end, Secret, [1, 2, 3]).

iterate(Secret, N) -> iterate_h([Secret], N - 1).

iterate_h(Accum, 0) -> lists:map(fun last_digit/1, lists:reverse(Accum));
iterate_h([Head | Accum], I) -> iterate_h([next_random(Head), Head | Accum], I - 1).

% This is quite inefficent...
last_digit(Price) -> lists:last(integer_to_list(Price)) - $0.

solve(Seeds) -> lists:map(fun (Secret) -> iterate(Secret, 2000) end, Seeds).

diffs([Head | Tail]) -> lists:zipwith(fun (X, Y) -> Y - X end, lists:droplast([Head | Tail]), Tail).

construct_map(Prices) ->
    Diffs = diffs(Prices),
    Tail = lists:nthtail(4, Prices),
    Tuples = lists:map(fun ({I, Elem}) -> {lists:sublist(Diffs, I, 4), Elem} end, enumerate(Tail)),
    maps:from_list(lists:reverse(Tuples)).

merge_maps([Head | Tail]) -> merge_maps(Tail, Head).
merge_maps([], Accum) -> Accum;
merge_maps([Head | Tail], Accum) ->
    merge_maps(Tail, maps:merge_with(fun (_, X, Y) -> X + Y end, Head, Accum)).

main() ->
    Lines = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    Seeds = lists:map(fun help:fst/1, (lists:map(fun string:to_integer/1, Lines))),
    Prices = lists:map(fun (Seed) -> iterate(Seed, 2000) end, Seeds),
    Maps = lists:map(fun construct_map/1, Prices),
    Merged = merge_maps(Maps),
    {_, Best} = maximum_by(fun maximum_fun/2, maps:to_list(Merged)),
    io:write(Best),
    io:nl(),
    halt().
