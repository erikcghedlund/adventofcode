-module(solve).
-export([main/0]).

-define(ALMOST, 0.0005).
-define(ALMOST_INTEGER(X), (((round(X) - ?ALMOST) =< X) and (X =< (round(X) + ?ALMOST)))).

solve({A, B, Target}) -> gausse(A, B, Target).


gausse({X1, X2}, {Y1, Y2}, {A1, A2}) ->
    Factor1 = X2 / X1,
    NewY2 = Y2 - Y1 * Factor1,
    NewA2 = A2 - A1 * Factor1,
    Factor2 = Y1 / NewY2,
    NewA1 = A1 - NewA2 * Factor2,
    RetX = round(NewA1 / X1),
    RetY = round(NewA2 / NewY2),
    if
        not ?ALMOST_INTEGER(NewA1 / X1) -> 0;
        not ?ALMOST_INTEGER(NewA2 / NewY2) -> 0;
        RetX < 0 -> 0;
        RetY < 0 -> 0;
        true -> RetX * 3 + RetY
    end.

parse_button(List) ->
    [_ | FirstStrInt] = lists:dropwhile(fun(C) -> C /= $+ end, List),
    {FirstInt, Rest} = string:to_integer(FirstStrInt),
    [_ | SecondStrInt] = lists:dropwhile(fun(C) -> C /= $+ end, Rest),
    {SecondInt, _} = string:to_integer(SecondStrInt),
    {FirstInt, SecondInt}.

parse_prize(List) ->
    [_ | FirstStrInt] = lists:dropwhile(fun(C) -> C /= $= end, List),
    {FirstInt, Rest} = string:to_integer(FirstStrInt),
    [_ | SecondStrInt] = lists:dropwhile(fun(C) -> C /= $= end, Rest),
    {SecondInt, _} = string:to_integer(SecondStrInt),
    {FirstInt + 10000000000000, SecondInt + 10000000000000}.

parse_input([]) ->
    [];
parse_input([Btn1, Btn2, Prz | Tail]) ->
    [
        {
            parse_button(Btn1),
            parse_button(Btn2),
            parse_prize(Prz)
        }
        | parse_input(Tail)
    ].

main() ->
    Lines = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    Problems = parse_input(Lines),
    Solutions = lists:map(fun solve/1, Problems),
    io:format("~p\n", [lists:sum(Solutions)]),
    halt().
