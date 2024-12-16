-module(solve).
-export([main/0]).

-define(WIDTH, 101).
-define(HEIGHT, 103).
-define(XINTER, ((?WIDTH) div 2)).
-define(YINTER, ((?HEIGHT) div 2)).

mod(X, Y) ->
    Rem = X rem Y,
    if
        Rem < 0 -> Y + Rem;
        true -> Rem
    end.

parse_robot(Line) ->
    [_ | FirstStrInt] = lists:dropwhile(fun(C) -> C /= $= end, Line),
    {FirstInt, [_ | Rest1]} = string:to_integer(FirstStrInt),
    {SecondInt, Rest2} = string:to_integer(Rest1),
    [_ | ThirdStrInt] = lists:dropwhile(fun(C) -> C /= $= end, Rest2),
    {ThirdInt, [_ | Rest3]} = string:to_integer(ThirdStrInt),
    {FourthInt, _} = string:to_integer(Rest3),
    {{FirstInt, SecondInt}, {ThirdInt, FourthInt}}.

move({{Xpos, Ypos}, {Xvel, Yvel}}, Time) ->
    {
        {
            mod((Xpos + Xvel * Time), ?WIDTH),
            mod((Ypos + Yvel * Time), ?HEIGHT)
        },
        {
            Xvel, Yvel
        }
    };
move([], _) ->
    [];
move([Robot | Robots], Time) ->
    [move(Robot, Time) | move(Robots, Time)].

parse_input([]) ->
    [];
parse_input([Line | Lines]) ->
    [parse_robot(Line) | parse_input(Lines)].

incr(X) -> X + 1.

partition(Robots) -> maps:remove(dc, partition(Robots, maps:new())).
partition([], Map) -> Map;
partition([{{Xpos, Ypos}, _} | Robots], Map) ->
    Category = if
                   (Xpos < ?XINTER) and (Ypos < ?YINTER) -> ul;
                   (Xpos < ?XINTER) and (Ypos > ?YINTER) -> bl;
                   (Xpos > ?XINTER) and (Ypos < ?YINTER) -> ur;
                   (Xpos > ?XINTER) and (Ypos > ?YINTER) -> br;
                   true -> dc
               end,
    NewMap = maps:update_with(Category, fun incr/1, 1, Map),
    partition(Robots, NewMap).

mul(List) -> lists:foldr(fun (X, Y) -> X * Y end, 1, List).
    
main() ->
    Lines = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    Robots = parse_input(Lines),
    Moved = move(Robots, 100),
    Partitioned = partition(Moved),
    io:format("~p\n", [mul(maps:values(Partitioned))]),
    halt().
