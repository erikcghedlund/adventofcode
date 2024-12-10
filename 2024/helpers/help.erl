-module(help).
-compile([export_all]).

delete_at(Index, List) -> lists:sublist(List, Index - 1) ++ lists:nthtail(Index, List).
fst({Fst, _}) -> Fst.
snd({_, Snd}) -> Snd.
lst2tup([X, Y]) -> {X, Y}.
stdin() -> stdin([]).
stdin(Accum) ->
    case io:get_chars('', 8192) of
        eof -> [Accum];
        Text -> stdin(Accum ++ Text)
    end.
count(Elem, List) -> length(lists:filter(fun(X) -> Elem == X end, List)).
chain(In, [Fun]) -> Fun(In);
chain(In, [Fun | Tail]) -> Fun(chain(In, Tail)).

transpose(Matrix) -> transpose(Matrix, 1).

transpose([Head | Matrix], Column) when Column > length(Matrix) ->
    [[lists:nth(Column, lists:nth(I, [Head | Matrix])) || {I, _} <- lists:enumerate(Head)]];
transpose([Head | Matrix], Column) ->
    [
        [lists:nth(Column, lists:nth(I, [Head | Matrix])) || {I, _} <- lists:enumerate(Head)]
        | transpose([Head | Matrix], Column + 1)
    ].
