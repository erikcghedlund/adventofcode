-module(solve).
-export([main/0]).

parse([Line | Tail]) ->
    Towels = lists:map(fun (Str) -> lists:delete($,, Str) end, string:lexemes(Line, [$\s])),
    {Towels, Tail}.

memoized_can_fold(_, []) -> [true];
memoized_can_fold(Towels, Pattern) -> case erlang:get({'memoized_can_fold', Towels, Pattern}) of
                                          Ans when is_list(Ans) -> Ans;
                                          'undefined' ->
    Help = fun (Towel) -> case lists:prefix(Towel, Pattern) of
     false -> [false];
     true -> memoized_can_fold(Towels, lists:nthtail(length(Towel), Pattern))
                                end
                end,

         lists:flatmap(Help, Towels)
                                      end.

% can_fold(Towels, []) -> [true];
% can_fold(Towels, Pattern) ->
%     Help = fun (Towel) -> case lists:prefix(Towel, Pattern) of
%      false -> [false];
%      true -> can_fold(Towels, lists:nthtail(length(Towel), Pattern))
%                                 end
%                 end,
% 
%          lists:flatmap(Help, Towels).


main() ->
    Lines = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    {Towels, Valids} = parse(Lines),
    Filtered = lists:flatmap(fun (Valid) -> memoized_can_fold(Towels, Valid) end, Valids),
    io:write(help:count(true, Filtered)), io:nl(),
    halt().
