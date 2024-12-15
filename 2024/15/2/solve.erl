-module(solve).
-export([main/0]).

-define(NTH(Matrix, X, Y), array:get(X, array:get(Y, Matrix))).
-define(HEIGHT(Matrix), array:size(Matrix)).
-define(WIDTH(Matrix), array:size(array:get(1, Matrix))).

sort_fun(up, {_, AY}, {_, BY}) -> AY > BY;
sort_fun(down, {_, AY}, {_, BY}) -> AY =< BY;
sort_fun(left, {AX, _}, {BX, _}) -> AX > BX;
sort_fun(right, {AX, _}, {BX, _}) -> AX =< BX.

lookahead(Matrix, Dir, {XPos, YPos}) ->
    {X, Y} = lookahead_help(Dir, XPos, YPos),
    ?NTH(Matrix, X, Y).

lookahead_help(up, X, Y) ->
    {X, Y - 1};
lookahead_help(down, X, Y) ->
    {X, Y + 1};
lookahead_help(left, X, Y) ->
    {X - 1, Y};
lookahead_help(right, X, Y) ->
    {X + 1, Y}.

thick([Head | Matrix]) when is_list(Head) ->
    plists:parmap(fun thick/1, [Head | Matrix]);
thick([]) ->
    [];
thick([$@ | Tail]) ->
    [$@, $. | thick(Tail)];
thick([$O | Tail]) ->
    [$[, $] | thick(Tail)];
thick([C | Tail]) ->
    [C, C | thick(Tail)].

char_to_dir($^) -> up;
char_to_dir($v) -> down;
char_to_dir($<) -> left;
char_to_dir($>) -> right.

matrix_set(Matrix, {XPos, YPos}, Val) ->
    array:set(YPos, array:set(XPos, Val, array:get(YPos, Matrix)), Matrix).

% Yeah fam I will not be able to keep myself DRY here...
can_push(Matrix, {X, Y}, Dir) when (Dir == up) or (Dir == down) ->
    YIncr =
        case Dir of
            down -> 1;
            up -> -1
        end,
    XIncr =
        case ?NTH(Matrix, X, Y) of
            $[ -> 1;
            $] -> -1
        end,
    [{X1, Y1}, {X2, Y2}] = lists:sort([{X, Y + YIncr}, {X + XIncr, Y + YIncr}]),
    case {?NTH(Matrix, X1, Y1), ?NTH(Matrix, X2, Y2)} of
        {$., $.} ->
            true;
        {$#, _} ->
            false;
        {_, $#} ->
            false;
        {$[, $]} ->
            can_push(Matrix, {X1, Y1}, Dir);
        {$], $[} ->
            lists:all(fun({X3, Y3}) -> can_push(Matrix, {X3, Y3}, Dir) end, [{X1, Y1}, {X2, Y2}]);
        {$], _} ->
            can_push(Matrix, {X1, Y1}, Dir);
        {_, $[} ->
            can_push(Matrix, {X2, Y2}, Dir)
    end;
can_push(Matrix, {X, Y}, Dir) when (Dir == left) or (Dir == right) ->
    Incr =
        case Dir of
            left -> -2;
            right -> 2
        end,
    case ?NTH(Matrix, X + Incr, Y) of
        $. -> true;
        $# -> false;
        _ -> can_push(Matrix, {X + Incr, Y}, Dir)
    end.

gather_boxes(Matrix, {X, Y}, Dir) when (Dir == up) or (Dir == down) ->
    YIncr =
        case Dir of
            down -> 1;
            up -> -1
        end,
    XIncr =
        case ?NTH(Matrix, X, Y) of
            $[ -> 1;
            $] -> -1
        end,
    [{X1, Y1}, {X2, Y2}] = lists:sort([{X, Y + YIncr}, {X + XIncr, Y + YIncr}]),
    case {?NTH(Matrix, X1, Y1), ?NTH(Matrix, X2, Y2)} of
        {$., $.} ->
            [{X, Y}];
        {$[, $]} ->
            [{X, Y} | gather_boxes(Matrix, {X1, Y1}, Dir)];
        {$], $[} ->
            [
                {X, Y}
                | lists:flatmap(fun({X3, Y3}) -> gather_boxes(Matrix, {X3, Y3}, Dir) end, [
                    {X1, Y1}, {X2, Y2}
                ])
            ];
        {$], _} ->
            [{X, Y} | gather_boxes(Matrix, {X1, Y1}, Dir)];
        {_, $[} ->
            [{X, Y} | gather_boxes(Matrix, {X2, Y2}, Dir)]
    end;
gather_boxes(Matrix, {X, Y}, Dir) when (Dir == left) or (Dir == right) ->
    Incr =
        case Dir of
            left -> -2;
            right -> 2
        end,
    case ?NTH(Matrix, X + Incr, Y) of
        $. -> [{X, Y}];
        _ -> [{X, Y} | gather_boxes(Matrix, {X + Incr, Y}, Dir)]
    end.

push(Matrix, [], _) ->
    Matrix;
push(Matrix, [{X, Y} | Tail], Dir) when (Dir == up) or (Dir == down) ->
    YIncr =
        case Dir of
            down -> 1;
            up -> -1
        end,
    XIncr =
        case ?NTH(Matrix, X, Y) of
            $[ -> 1;
            $] -> -1
        end,
    [{X1, Y1}, {X2, Y2}] = lists:sort([{X, Y + YIncr}, {X + XIncr, Y + YIncr}]),
    Matrix0 = push(Matrix, Tail, Dir),
    Matrix1 = matrix_set(Matrix0, {X1, Y1}, $[),
    Matrix2 = matrix_set(Matrix1, {X2, Y2}, $]),
    Matrix3 = matrix_set(Matrix2, {X1, Y}, $.),
    matrix_set(Matrix3, {X2, Y}, $.);
push(Matrix, [{X, Y} | Tail], left) ->
    Matrix0 = push(Matrix, Tail, left),
    Matrix1 = matrix_set(Matrix0, {X - 2, Y}, $[),
    Matrix2 = matrix_set(Matrix1, {X - 1, Y}, $]),
    matrix_set(Matrix2, {X, Y}, $.);
push(Matrix, [{X, Y} | Tail], right) ->
    Matrix0 = push(Matrix, Tail, right),
    Matrix1 = matrix_set(Matrix0, {X + 2, Y}, $]),
    Matrix2 = matrix_set(Matrix1, {X + 1, Y}, $[),
    matrix_set(Matrix2, {X, Y}, $.).

move(Matrix, [], Pos) ->
    matrix_set(Matrix, Pos, $@);
move(Matrix, [Char | Tail], {XPos, YPos}) ->
    Dir = char_to_dir(Char),
    Next = lookahead_help(Dir, XPos, YPos),
    verify_area(Matrix),
    case lookahead(Matrix, Dir, {XPos, YPos}) of
        $# ->
            move(Matrix, Tail, {XPos, YPos});
        $. ->
            move(Matrix, Tail, Next);
        _ ->
            case can_push(Matrix, Next, Dir) of
                false ->
                    move(Matrix, Tail, {XPos, YPos});
                true ->
                    % If we push in the wrong order, bugs will occur...
                    Boxes = lists:sort(
                        fun(A, B) -> sort_fun(Dir, A, B) end, gather_boxes(Matrix, Next, Dir)
                    ),
                    NewMatrix = push(Matrix, Boxes, Dir),
                    move(NewMatrix, Tail, Next)
            end
    end.

verify_area(Matrix) ->
    case verify_integrity(array:to_list(Matrix)) of
        true ->
            ok;
        false ->
            io:fwrite("error\n"),
            halt()
    end.

verify_integrity([Row | Matrix]) ->
    lists:all(fun(Arr) -> verify_integrity_h(array:to_list(Arr)) end, [Row | Matrix]).

verify_integrity_h([]) -> true;
verify_integrity_h([$[, $. | _]) -> false;
verify_integrity_h([$., $] | _]) -> false;
verify_integrity_h([$[, $# | _]) -> false;
verify_integrity_h([$#, $] | _]) -> false;
verify_integrity_h([_ | Tail]) -> verify_integrity_h(Tail).

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
        $[ -> XPos + YPos * 100;
        _ -> 0
    end.

main() ->
    Lines = lists:flatten(help:stdin()),
    {Map, Moves} = lists:splitwith(fun(C) -> not lists:member(C, [$<, $>, $^, $v]) end, Lines),
    Matrix = thick(string:lexemes(Map, [$\n])),
    Start = find_start(Matrix),
    Area = construct_map(Matrix, Start),
    FilteredMoves = lists:filter(fun(C) -> lists:member(C, [$<, $>, $^, $v]) end, Moves),
    FinalArea = move(Area, FilteredMoves, Start),
    draw_area(FinalArea),
    io:format("~p\n", [
        lists:sum(
            plists:parmap(fun(Pos) -> gps(FinalArea, Pos) end, [
                {X, Y}
             || X <- lists:seq(1, ?WIDTH(FinalArea) - 1), Y <- lists:seq(1, ?HEIGHT(FinalArea) - 1)
            ])
        )
    ]),
    halt().
