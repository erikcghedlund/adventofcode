-module(solve).
-export([main/0]).

-define(D(C), (($0 =< C) and (C =< $9))).
-define(DIGITS(C1, C2), ?D(C1) and ?D(C2)).
-define(DIGITS(C1, C2, C3), ?D(C1) and ?D(C2) and ?D(C3)).
-define(DIGITS(C1, C2, C3, C4), ?D(C1) and ?D(C2) and ?D(C3) and ?D(C4)).
-define(DIGITS(C1, C2, C3, C4, C5), ?D(C1) and ?D(C2) and ?D(C3) and ?D(C4) and ?D(C5)).
-define(DIGITS(C1, C2, C3, C4, C5, C6), ?D(C1) and ?D(C2) and ?D(C3) and ?D(C4) and ?D(C5) and ?D(C6)).

parse([$m, $u, $l, $(, C1, C2, C3, $,, C4, C5, C6, $) | Tail]) when ?DIGITS(C1, C2, C3, C4, C5, C6) -> [help:fst(string:to_integer([C1, C2, C3])) * help:fst(string:to_integer([C4, C5, C6])) | parse(Tail)];
parse([$m, $u, $l, $(, C1, C2, C3, $,, C4, C5, $) | Tail]) when ?DIGITS(C1, C2, C3, C4, C5) -> [help:fst(string:to_integer([C1, C2, C3])) * help:fst(string:to_integer([C4, C5])) | parse(Tail)];
parse([$m, $u, $l, $(, C1, C2, $,, C4, C5, C6, $) | Tail]) when ?DIGITS(C1, C2, C4, C5, C6) -> [help:fst(string:to_integer([C1, C2])) * help:fst(string:to_integer([C4, C5, C6])) | parse(Tail)];
parse([$m, $u, $l, $(, C1, C2, C3, $,, C4, $) | Tail]) when ?DIGITS(C1, C2, C3, C4) -> [help:fst(string:to_integer([C1, C2, C3])) * help:fst(string:to_integer([C4])) | parse(Tail)];
parse([$m, $u, $l, $(, C1, $,, C4, C5, C6, $) | Tail]) when ?DIGITS(C1, C4, C5, C6) -> [help:fst(string:to_integer([C1])) * help:fst(string:to_integer([C4, C5, C6])) | parse(Tail)];
parse([$m, $u, $l, $(, C1, C2, $,, C4, C5, $) | Tail]) when ?DIGITS(C1, C2, C4, C5) -> [help:fst(string:to_integer([C1, C2])) * help:fst(string:to_integer([C4, C5])) | parse(Tail)];
parse([$m, $u, $l, $(, C1, C2, $,, C4, $) | Tail]) when ?DIGITS(C1, C2, C4) -> [help:fst(string:to_integer([C1, C2])) * help:fst(string:to_integer([C4])) | parse(Tail)];
parse([$m, $u, $l, $(, C1, $,, C4, C5, $) | Tail]) when ?DIGITS(C1, C4, C5) -> [help:fst(string:to_integer([C1])) * help:fst(string:to_integer([C4, C5])) | parse(Tail)];
parse([$m, $u, $l, $(, C1, $,, C4, $) | Tail]) when ?DIGITS(C1, C4) -> [help:fst(string:to_integer([C1])) * help:fst(string:to_integer([C4])) | parse(Tail)];
parse([_ | Tail]) -> parse(Tail);
parse([]) -> [].

skip_dont(_, [$d, $o, $n, $', $t, $(, $) | Tokens], Accum) -> skip_dont(false, Tokens, Accum);
skip_dont(_, [$d, $o, $(, $) | Tokens], Accum) -> skip_dont(true, Tokens, Accum);
skip_dont(true, [H | Tokens], Accum) -> skip_dont(true, Tokens, [H | Accum]);
skip_dont(false, [_ | Tokens], Accum) -> skip_dont(false, Tokens, Accum);
skip_dont(_, [], Accum) -> lists:reverse(Accum).
skip_dont(Tokens) -> skip_dont(true, Tokens, []).

main() ->
    In = lists:flatten(help:stdin()),
    Skipped = skip_dont(In),
    Factors = parse(Skipped),
    io:format("~p\n", [lists:sum(Factors)]), halt().
