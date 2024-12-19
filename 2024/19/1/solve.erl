-module(solve).
-export([main/0]).

parse([Line | Tail]) ->
    Towels = lists:map(fun (Str) -> lists:delete($,, Str) end, string:lexemes(Line, [$\s])),
    {Towels, Tail}.

can_fold(Towels, Pattern) -> case lists:member(Pattern, Towels) of
                                 true -> true;
                                 false -> lists:any(fun (Towel) -> lists:prefix(Towel, Pattern) andalso can_fold(Towels, lists:subtract(Pattern, Towel)) end, Towels)
                            end.


main() ->
    Lines = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    {Towels, Valids} = parse(Lines),
    Filtered = lists:filter(fun (Valid) -> can_fold(Towels, Valid) end, Valids),
    io:write(length(Filtered)), io:nl(),
    halt().
