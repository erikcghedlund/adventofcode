-module(solve).
-export([main/0]).

-define(NTH(Matrix, X, Y), array:get(X, array:get(Y, Matrix))).
-define(HEIGHT(Matrix), array:size(Matrix)).
-define(WIDTH(Matrix), array:size(array:get(1, Matrix))).

lookahead(Matrix, Dir, XPos, YPos) ->
    {X, Y} = lookahead_help(Matrix, Dir, XPos, YPos),
    case ?NTH(Matrix, X, Y) of
        $# -> false;
        $. -> {X, Y}
    end.

lookahead_help(Matrix, up, XPos, YPos) ->
    help:snd(
        lists:search(fun({X, Y}) -> ?NTH(Matrix, X, Y) /= $O end, [
            {XPos, Y}
         || Y <- lists:seq(YPos - 1, 0, -1)
        ])
    );
lookahead_help(Matrix, down, XPos, YPos) ->
    help:snd(
        lists:search(fun({X, Y}) -> ?NTH(Matrix, X, Y) /= $O end, [
            {XPos, Y}
         || Y <- lists:seq(YPos + 1, ?HEIGHT(Matrix) - 1)
        ])
    );
lookahead_help(Matrix, left, XPos, YPos) ->
    help:snd(
        lists:search(fun({X, Y}) -> ?NTH(Matrix, X, Y) /= $O end, [
            {X, YPos}
         || X <- lists:seq(XPos - 1, 0, -1)
        ])
    );
lookahead_help(Matrix, right, XPos, YPos) ->
    help:snd(
        lists:search(fun({X, Y}) -> ?NTH(Matrix, X, Y) /= $O end, [
            {X, YPos}
         || X <- lists:seq(XPos + 1, ?WIDTH(Matrix) - 1)
        ])
    ).

char_to_dir($^) -> up;
char_to_dir($v) -> down;
char_to_dir($<) -> left;
char_to_dir($>) -> right.

move_help(Matrix, {XFrom, YFrom}, {XTo, YTo}, _) when {XFrom, YFrom} == {XTo, YTo} ->
    array:set(YFrom, array:set(XFrom, $O, array:get(YFrom, Matrix)), Matrix);
move_help(Matrix, {XFrom, YFrom}, {XTo, YTo}, Mode) when XFrom == XTo ->
    ThisChar =
        case Mode of
            entry -> $.;
            recurse -> $O
        end,
    NewMatrix =
        if
            YFrom > YTo -> move_help(Matrix, {XFrom, YFrom - 1}, {XTo, YTo}, recurse);
            true -> move_help(Matrix, {XFrom, YFrom + 1}, {XTo, YTo}, recurse)
        end,
    array:set(YFrom, array:set(XFrom, ThisChar, array:get(YFrom, NewMatrix)), NewMatrix);
move_help(Matrix, {XFrom, YFrom}, {XTo, YTo}, Mode) when YFrom == YTo ->
    ThisChar =
        case Mode of
            entry -> $.;
            recurse -> $O
        end,
    NewMatrix =
        if
            XFrom > XTo -> move_help(Matrix, {XFrom - 1, YFrom}, {XTo, YTo}, recurse);
            true -> move_help(Matrix, {XFrom + 1, YFrom}, {XTo, YTo}, recurse)
        end,
    array:set(YFrom, array:set(XFrom, ThisChar, array:get(YFrom, NewMatrix)), NewMatrix).

move(Matrix, [], XPos, YPos) ->
    array:set(YPos, array:set(XPos, $@, array:get(YPos, Matrix)), Matrix);
move(Matrix, [Dir | Tail], XPos, YPos) ->
    case lookahead(Matrix, char_to_dir(Dir), XPos, YPos) of
        false ->
            move(Matrix, Tail, XPos, YPos);
        {X, Y} ->
            NewMatrix = move_help(Matrix, {XPos, YPos}, {X, Y}, entry),
            {NewXPos, NewYPos} =
                case char_to_dir(Dir) of
                    up -> {XPos, YPos - 1};
                    down -> {XPos, YPos + 1};
                    left -> {XPos - 1, YPos};
                    right -> {XPos + 1, YPos}
                end,
            move(NewMatrix, Tail, NewXPos, NewYPos)
    end.

find_start(Matrix) ->
    {_, {Y, Row}} = lists:search(fun({_, R}) -> lists:member($@, R) end, lists:enumerate(Matrix)),
    {_, {X, _}} = lists:search(fun({_, R}) -> $@ == R end, lists:enumerate(Row)),
    {X - 1, Y - 1}.

construct_map(Matrix, {XPos, YPos}) ->
    RetArray = array:from_list(lists:map(fun array:from_list/1, Matrix)),
    array:set(YPos, array:set(XPos, $., array:get(YPos, RetArray)), RetArray).

draw_area(Matrix) ->
    ListMatrix = array:to_list(Matrix),
    lists:foreach(
        fun(Arr) ->
            io:fwrite(array:to_list(Arr)),
            io:nl()
        end,
        ListMatrix
    ).

gps(Matrix, {XPos, YPos}) ->
    case ?NTH(Matrix, XPos, YPos) of
        $O -> XPos + YPos * 100;
        _ -> 0
    end.

main() ->
    Lines = lists:flatten(help:stdin()),
    {Map, Moves} = lists:splitwith(fun(C) -> not lists:member(C, [$<, $>, $^, $v]) end, Lines),
    Matrix = string:lexemes(Map, [$\n]),
    {StartX, StartY} = find_start(Matrix),
    Area = construct_map(Matrix, {StartX, StartY}),
    FilteredMoves = lists:filter(fun(C) -> lists:member(C, [$<, $>, $^, $v]) end, Moves),
    FinalArea = move(Area, FilteredMoves, StartX, StartY),
    io:format("~p\n", [
        lists:sum(
            plists:parmap(fun(Pos) -> gps(FinalArea, Pos) end, [
                {X, Y}
             || X <- lists:seq(1, ?WIDTH(FinalArea) - 1), Y <- lists:seq(1, ?HEIGHT(FinalArea) - 1)
            ])
        )
    ]),
    halt().
