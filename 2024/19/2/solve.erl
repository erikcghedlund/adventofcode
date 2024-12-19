-module(solve).
-export([main/0]).
-define(MEMOIZED,).

parse([Line | Tail]) ->
    Towels = lists:map(fun(Str) -> lists:delete($,, Str) end, string:lexemes(Line, [$\s])),
    {Towels, Tail}.

-ifdef(MEMOIZED).
can_fold(_, []) ->
    1;
can_fold(Towels, Pattern) ->
    case erlang:get({'memoized_can_fold', Towels, Pattern}) of
        Ans when is_integer(Ans) ->
            Ans;
        'undefined' ->
            Help = fun(Towel) ->
                case lists:prefix(Towel, Pattern) of
                    false -> 0;
                    true -> can_fold(Towels, lists:nthtail(length(Towel), Pattern))
                end
            end,
            Res = lists:sum(lists:map(Help, Towels)),
            erlang:put({'memoized_can_fold', Towels, Pattern}, Res),
            Res
    end.
-else.
can_fold(_, []) ->
    1;
can_fold(Towels, Pattern) ->
    Help = fun(Towel) ->
        case lists:prefix(Towel, Pattern) of
            false -> 0;
            true -> can_fold(Towels, lists:nthtail(length(Towel), Pattern))
        end
    end,
    lists:sum(lists:map(Help, Towels)).
-endif.

solve(_, [], Accum) ->
    Accum;
solve(Towels, [Head | Tail], Accum) ->
    Res = can_fold(Towels, Head),
    solve(Towels, Tail, Accum + Res).

main() ->
    Lines = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    {Towels, Valids} = parse(Lines),
    Res = solve(Towels, Valids, 0),
    io:write(Res),
    io:nl(),
    halt().
