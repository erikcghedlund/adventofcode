-module(solve).
-export([main/0]).

iters(Ax, Bx, Targetx) when Ax < Bx -> {a, Targetx div Ax};
iters(_, Bx, Targetx) -> {b, Targetx div Bx}.

cheaper(_, {Ai, Bi}, {Aj, Bj}) -> (Ai*3 + Bi) =< (Aj*3 + Bj).

% this is quite inefficent, might optimize later.
min_by(Fun, List) ->
    [Ret | _] = lists:sort(Fun, List),
    Ret.

filter_fun(b, I, {Ax, Ay}, {Bx, By}, {Targetx, Targety}) ->
    if
        (Targetx - (Bx * I)) rem Ax /= 0 -> false;
        (Targety - (By * I)) < 0 -> false;
        (Targety - (By * I)) rem Ay /= 0 -> false;
        (Targetx - (Bx * I)) div Ax /= (Targety - (By * I)) div Ay -> false;
        true -> {true, {(Targetx - (Bx * I)) div Ax, I}}
    end;
filter_fun(a, I, {Ax, Ay}, {Bx, By}, {Targetx, Targety}) ->
    if
        (Targetx - (Ax * I)) rem Bx /= 0 -> false;
        (Targety - (Ay * I)) < 0 -> false;
        (Targety - (Ay * I)) rem By /= 0 -> false;
        (Targetx - (Ax * I)) div Bx /= (Targety - (Ay * I)) div By -> false;
        true -> {true, {I, (Targetx - (Ax * I)) div Bx}}
    end.

solve({A, B, Target}) -> solve(A, B, Target).

solve({Ax, Ay}, {Bx, By}, {Targetx, Targety}) ->
    {Char, Iterations} = iters(Ax, Bx, Targetx),
    Candidates = lists:seq(0, Iterations),
    case lists:filtermap(fun (I) -> filter_fun(Char, I, {Ax, Ay}, {Bx, By}, {Targetx, Targety}) end, Candidates) of
        [] -> 0;
        PotentialSols ->
            io:write(PotentialSols), io:nl(),
            Cheaper = fun (A, B) -> cheaper(Char, A, B) end,
            {BestX, BestY} = min_by(Cheaper, PotentialSols),
            io:fwrite("Best = ~p\n", [{BestX, BestY}]),
            io:fwrite("Test = ~p = ~p\n ~p = ~p\n", [BestX*Ax + BestY*Bx, Targetx,BestX*Ay + BestY*By, Targety]),
            BestX*3 + BestY
    end.

parse_button(List) ->
    [_ | FirstStrInt] = lists:dropwhile(fun (C) -> C /= $+ end, List),
    {FirstInt, Rest} = string:to_integer(FirstStrInt),
    [_ | SecondStrInt] = lists:dropwhile(fun (C) -> C /= $+ end, Rest),
    {SecondInt, _}  = string:to_integer(SecondStrInt),
    {FirstInt, SecondInt}.

parse_prize(List) ->
    [_ | FirstStrInt] = lists:dropwhile(fun (C) -> C /= $= end, List),
    {FirstInt, Rest} = string:to_integer(FirstStrInt),
    [_ | SecondStrInt] = lists:dropwhile(fun (C) -> C /= $= end, Rest),
    {SecondInt, _}  = string:to_integer(SecondStrInt),
    {FirstInt, SecondInt}.

parse_input([]) -> [];
parse_input([Btn1, Btn2, Prz | Tail]) -> [{
                                 parse_button(Btn1),
                                 parse_button(Btn2),
                                 parse_prize(Prz)
                                 }| parse_input(Tail)
                                ].


main() ->
    Lines = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    Problems = parse_input(Lines),
    Solutions = lists:map(fun solve/1, Problems),
    io:format("~p\n", [lists:sum(Solutions)]),
    halt().
