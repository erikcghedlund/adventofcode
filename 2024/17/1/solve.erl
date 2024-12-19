-module(solve).
-export([main/0]).

-define(DIGIT(C), (($0 =< C) and (C =< $9))).

mod(X, Y) ->
    Rem = X rem Y,
    if
        Rem < 0 -> Y + Rem;
        true -> Rem
    end.

get_fun(0) -> fun adv/5;
get_fun(1) -> fun bxl/5;
get_fun(2) -> fun bst/5;
get_fun(3) -> fun jnz/5;
get_fun(4) -> fun bxc/5;
get_fun(5) -> fun out/5;
get_fun(6) -> fun bdv/5;
get_fun(7) -> fun cdv/5.

combo(X, _, _, _) when (0 =< X) and (X =< 3) -> X;
combo(4, A, _, _) -> A;
combo(5, _, B, _) -> B;
combo(6, _, _, C) -> C.

adv(Operand, A, B, C, Pointer) -> {{A div (1 bsl combo(Operand, A, B, C)), B, C}, Pointer + 2}.
bxl(Operand, A, B, C, Pointer) -> {{A, B bxor Operand, C}, Pointer + 2}.
bst(Operand, A, B, C, Pointer) -> {{A, mod(combo(Operand, A, B, C), 8), C}, Pointer + 2}.
jnz(_, 0, B, C, Pointer) -> {{0, B, C}, Pointer + 2};
jnz(Operand, A, B, C, Pointer) -> {{A, B, C}, Operand}.
bxc(_, A, B, C, Pointer) -> {{A, B bxor C, C}, Pointer + 2}.
out(Operand, A, B, C, Pointer) ->
    {{NewA, NewB, NewC}, NewPointer} = bst(Operand, A, B, C, Pointer),
    io:fwrite("~p,", [NewB]),
    {{NewA, NewB, NewC}, NewPointer}.
bdv(Operand, A, B, C, Pointer) -> {{A, A div (1 bsl combo(Operand, A, B, C)), C}, Pointer + 2}.
cdv(Operand, A, B, C, Pointer) -> {{A, B, A div (1 bsl combo(Operand, A, B, C))}, Pointer + 2}.

parse_prog([]) -> [];
parse_prog([Head | Tail]) when ?DIGIT(Head) ->
    {Int1, [_ | Rest1]} = string:to_integer([Head | Tail]),
    {Int2, Rest2} = string:to_integer(Rest1),
    [Int1, Int2 | parse_prog(Rest2)];
parse_prog([_ | Tail]) -> parse_prog(Tail).


parse([LineA, LineB, LineC, LineProg]) ->
    {A, _} = string:to_integer(lists:dropwhile(fun (Char) -> not ?DIGIT(Char) end, LineA)),
    {B, _} = string:to_integer(lists:dropwhile(fun (Char) -> not ?DIGIT(Char) end, LineB)),
    {C, _} = string:to_integer(lists:dropwhile(fun (Char) -> not ?DIGIT(Char) end, LineC)),
    Prog = array:from_list(parse_prog(LineProg)),
    {A, B, C, Prog}.

execute(Program, A, B, C, Pointer) -> case array:size(Program) =< Pointer of
                                          true -> exit;
                                          false ->
                                              Fun = get_fun(array:get(Pointer, Program)),
                                              Operand = array:get(Pointer + 1, Program),
                                              {{NewA, NewB, NewC}, NewPointer} = Fun(Operand, A, B, C, Pointer),
                                              %io:fwrite("New program state: ~p\n", [{{NewA, NewB, NewC}, NewPointer}]),
                                              execute(Program, NewA, NewB, NewC, NewPointer)
                                      end.



main() ->
    Lines = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    {A, B, C, Program} = parse(Lines),
    execute(Program, A, B, C, 0),
    halt().
