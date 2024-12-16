-module(solve).
-export([main/0]).

-ifdef(TEST).
-define(WIDTH, 11).
-define(HEIGHT, 7).
-else.
-define(WIDTH, 101).
-define(HEIGHT, 103).
-define(HOTALCOMBS, 10403).
-endif.
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

draw_str([], _) ->
    [];
draw_str([{X, _} | Iters], Map) when X == ?WIDTH -> [$\n | draw_str(Iters, Map)];
draw_str([Pos | Iters], Map) ->
    case maps:get(Pos, Map) of
        0 -> [$\. | draw_str(Iters, Map)];
        X -> [X + $0 | draw_str(Iters, Map)]
    end.

sort_fun({X1, X2}, {Y1, Y2}) when X2 == Y2 -> X1 =< Y1;
sort_fun({_, X2}, {_, Y2}) -> X2 =< Y2.

draw_all(Robots) -> draw_all(Robots, 0, sets:new()).

draw_all(Robots, I, Prev) ->
    io:fwrite("Time = ~p\n\n", [I]),
    Moved = move(Robots, I),
    case sets:is_element(Moved, Prev) of
        true ->
            io:fwrite("We've been here before, terminating...\n");
        false ->
            draw(Moved),
            draw_all(Robots, I + 1, sets:add_element(Moved, Prev))
    end.

draw(Robots) ->
    Positions = lists:map(fun help:fst/1, Robots),
    Iters = lists:sort(fun sort_fun/2, [
        {X, Y}
     || X <- lists:seq(0, ?WIDTH), Y <- lists:seq(0, ?HEIGHT - 1)
    ]),
    Map = maps:from_list(lists:map(fun(X) -> {X, help:count(X, Positions)} end, Iters)),
    io:fwrite(lists:duplicate(?WIDTH, $=)),
    io:nl(),
    io:fwrite(draw_str(Iters, Map)),
    io:fwrite(lists:duplicate(?WIDTH, $=)),
    io:nl().

main() ->
    Lines = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    Robots = parse_input(Lines),
    draw_all(Robots),
    halt().
