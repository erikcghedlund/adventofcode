-module(solve).
-export([main/0]).

-define(NTH(Matrix, X, Y), lists:nth(X, lists:nth(Y, Matrix))).

construct_map(Lines) -> construct_map(Lines, []).
construct_map([], Accum) ->
    Accum;
construct_map([Line | Lines], Accum) ->
    [Key, Factors] = string:lexemes(Line, [$:]),
    {IntKey, _} = string:to_integer(Key),
    IntFactors = lists:map(
        fun(Int) -> help:fst(string:to_integer(string:trim(Int))) end,
        string:lexemes(Factors, [$\s])
    ),
    construct_map(Lines, [{IntKey, IntFactors} | Accum]).

eval([Head | Tail]) -> eval(Tail, [Head]).
eval([Int], Accum) ->
    lists:map(fun(X) -> X + Int end, Accum) ++ lists:map(fun(X) -> X * Int end, Accum);
eval([Head | Tail], Accum) ->
    eval(
        Tail, lists:map(fun(X) -> X + Head end, Accum) ++ lists:map(fun(X) -> X * Head end, Accum)
    ).

main() ->
    Lines = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    Map = construct_map(Lines),
    Filtered = lists:filter(fun({Key, Factors}) -> lists:member(Key, eval(Factors)) end, Map),
    io:format("~p\n", [lists:sum(lists:map(fun help:fst/1, Filtered))]),
    halt().
