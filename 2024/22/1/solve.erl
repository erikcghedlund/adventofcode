-module(solve).
-export([main/0]).
-define(MODNUM, 16777216).

mix(Secret, Val) -> Secret bxor Val.

prune(Secret) -> Secret rem ?MODNUM.

step(1, Secret) -> prune(mix(Secret, Secret * 64));
step(2, Secret) -> prune(mix(Secret, Secret div 32));
step(3, Secret) -> prune(mix(Secret, Secret * 2048)).

next_random(Secret) -> lists:foldl(fun(I, Acc) -> step(I, Acc) end, Secret, [1, 2, 3]).

iterate(Secret, N) -> lists:foldl(fun(_, Acc) -> next_random(Acc) end, Secret, lists:seq(1, N)).

solve(Seeds) -> lists:sum(lists:map(fun(Secret) -> iterate(Secret, 2000) end, Seeds)).

main() ->
    Lines = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    Seeds = lists:map(fun help:fst/1, (lists:map(fun string:to_integer/1, Lines))),
    io:write(solve(Seeds)),
    io:nl(),
    halt().
