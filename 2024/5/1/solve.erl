-module(solve).
-export([main/0]).

construct_map(Lines) -> construct_map(Lines, maps:new()).
construct_map([], Accum) ->
    Accum;
construct_map([Head | Lines], Accum) ->
    [Val, Before] = lists:map(
        fun(X) -> help:fst(string:to_integer(X)) end, string:lexemes(Head, [$|])
    ),
    NewMap = maps:put(Val, [Before | maps:get(Val, Accum, [])], Accum),
    construct_map(Lines, NewMap).

manual_is_valid(AfterMap, Manual) ->
    lists:all(
        fun({Index, Elem}) -> manual_is_valid_helper(AfterMap, Index, Elem, Manual) end,
        lists:enumerate(Manual)
    ).

manual_is_valid_helper(AfterMap, Index, Elem, List) ->
    Past = lists:sublist(List, Index - 1),
    Forbidden = maps:get(Elem, AfterMap, []),
    sets:is_disjoint(sets:from_list(Past), sets:from_list(Forbidden)).

main() ->
    Lines = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    {Pipes, Manuals} = lists:splitwith(fun(S) -> not lists:member($,, S) end, Lines),
    AfterMap = construct_map(Pipes),
    ConvertedManuals = lists:map(
        fun(Manual) ->
            lists:map(
                fun(S) -> help:fst(string:to_integer(string:trim(S))) end,
                string:lexemes(Manual, [$,])
            )
        end,
        Manuals
    ),
    ValidManuals = lists:filter(
        fun(Manual) -> manual_is_valid(AfterMap, Manual) end, ConvertedManuals
    ),
    io:format("~p\n", [
        lists:sum([lists:nth((length(List) div 2 + 1), List) || List <- ValidManuals])
    ]),
    halt().
