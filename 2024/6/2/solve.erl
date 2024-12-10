-module(solve).
-export([main/0]).

-define(NTH(Matrix, X, Y), lists:nth(X, lists:nth(Y, Matrix))).

lookahead(Matrix, up, XPos, YPos) -> lookahead_help(Matrix, XPos, YPos + 1);
lookahead(Matrix, down, XPos, YPos) -> lookahead_help(Matrix, XPos, YPos - 1);
lookahead(Matrix, left, XPos, YPos) -> lookahead_help(Matrix, XPos - 1, YPos);
lookahead(Matrix, right, XPos, YPos) -> lookahead_help(Matrix, XPos + 1, YPos).

lookahead_help(_, 0, _) ->
    out;
lookahead_help(_, _, 0) ->
    out;
lookahead_help([Head | _], X, _) when length(Head) < X -> out;
lookahead_help(Matrix, _, Y) when length(Matrix) < Y -> out;
lookahead_help(Matrix, X, Y) ->
    case ?NTH(Matrix, X, Y) of
        $# -> obstructed;
        _ -> free
    end.

turn(up) -> right;
turn(right) -> down;
turn(down) -> left;
turn(left) -> up.

move(Matrix, up, XPos, YPos, Passed) -> check(Matrix, up, XPos, YPos + 1, Passed);
move(Matrix, down, XPos, YPos, Passed) -> check(Matrix, down, XPos, YPos - 1, Passed);
move(Matrix, right, XPos, YPos, Passed) -> check(Matrix, right, XPos + 1, YPos, Passed);
move(Matrix, left, XPos, YPos, Passed) -> check(Matrix, left, XPos - 1, YPos, Passed).

check(Matrix, Direction, XPos, YPos, Passed) ->
    case sets:is_element({Direction, XPos, YPos}, Passed) of
        true ->
            stuck;
        false ->
            case lookahead(Matrix, Direction, XPos, YPos) of
                out ->
                    out;
                obstructed ->
                    check(Matrix, turn(Direction), XPos, YPos, Passed);
                free ->
                    move(
                        Matrix,
                        Direction,
                        XPos,
                        YPos,
                        sets:add_element({Direction, XPos, YPos}, Passed)
                    )
            end
    end.

find_start(Matrix) ->
    {_, {Y, Row}} = lists:search(fun({_, R}) -> lists:member($^, R) end, lists:enumerate(Matrix)),
    {_, {X, _}} = lists:search(fun({_, R}) -> $^ == R end, lists:enumerate(Row)),
    {X, Y}.

obstruct(Matrix, X, Y) ->
    {KeepHead, [ToMutate | KeepTail]} = lists:split(Y - 1, Matrix),
    {KeepHead2, [_ | KeepTail2]} = lists:split(X - 1, ToMutate),
    New =
        KeepHead ++
            [KeepHead2 ++ [$# | KeepTail2]] ++
            KeepTail,
    New.

main() ->
    Matrix = lists:reverse(string:lexemes(lists:flatten(help:stdin()), [$\n])),
    {X, Y} = find_start(Matrix),
    CrossProduct = [
        {XX, YY}
     || {XX, _} <- lists:enumerate(lists:nth(1, Matrix)), {YY, _} <- lists:enumerate(Matrix)
    ],
    Candidates = lists:filter(fun({XX, YY}) -> ?NTH(Matrix, XX, YY) == $. end, CrossProduct),
    Escapes = lists:map(
        fun({XX, YY}) -> check(obstruct(Matrix, XX, YY), up, X, Y, sets:new()) end, Candidates
    ),
    io:format("~p\n", [help:count(stuck, Escapes)]),
    halt().
