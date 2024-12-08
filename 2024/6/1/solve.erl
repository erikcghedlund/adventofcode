-module(solve).
-export([main/0]).

-define(NTH(Matrix, X, Y), lists:nth(X, lists:nth(Y, Matrix))).

lookahead(Matrix, up, XPos, YPos) -> lookahead_help(Matrix, XPos, YPos + 1);
lookahead(Matrix, down, XPos, YPos) -> lookahead_help(Matrix, XPos, YPos - 1);
lookahead(Matrix, left, XPos, YPos) -> lookahead_help(Matrix, XPos - 1, YPos);
lookahead(Matrix, right, XPos, YPos) -> lookahead_help(Matrix, XPos + 1, YPos).

lookahead_help(_, 0, _) -> out;
lookahead_help(_, _, 0) -> out;
lookahead_help([Head, _], X, _) when length(Head) < X -> out;
lookahead_help(Matrix, _, Y) when length(Matrix) < Y -> out;
lookahead_help(Matrix, X, Y) -> case ?NTH(Matrix, X, Y) of
                                    $# -> obstructed;
                                    _ -> free
                                end.

turn(up) -> right;
turn(right) -> down;
turn(down) -> left;
turn(left) -> up.

move(Matrix, up, XPos, YPos) -> [{XPos, YPos} | check(Matrix, up, XPos, YPos + 1)];
move(Matrix, down, XPos, YPos) -> [{XPos, YPos} | check(Matrix, down, XPos, YPos - 1)];
move(Matrix, right, XPos, YPos) -> [{XPos, YPos} | check(Matrix, right, XPos + 1, YPos)];
move(Matrix, left, XPos, YPos) -> [{XPos, YPos} | check(Matrix, left, XPos - 1, YPos)].

check(Matrix, Direction, XPos, YPos) -> case lookahead(Matrix, Direction, XPos, YPos) of
                                            out -> [{XPos, YPos}];
                                            obstructed -> check(Matrix, turn(Direction), XPos, YPos);
                                            free -> move(Matrix, Direction, XPos,  YPos)
                                        end.

find_start(Matrix) ->
    {_, {Y, Row}} = lists:search(fun ({_, R}) -> lists:member($^, R) end, lists:enumerate(Matrix)),
    {_, {X, _}} = lists:search(fun ({_, R}) -> $^ == R end, lists:enumerate(Row)),
    {X, Y}.
                                            

main() ->
    Matrix = lists:reverse(string:lexemes(lists:flatten(help:stdin()), [$\n])),
    {X, Y} = find_start(Matrix),
    Path = check(Matrix, up, X, Y),
    io:format("~p\n", [length(lists:uniq(Path))]),
    halt().
