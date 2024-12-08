-module(solve).
-export([solve/0]).


fst({Fst, _}) -> Fst.
snd({_, Snd}) -> Snd.
lst2tup([X, Y]) -> {X, Y}.

solve() ->
    {Col1, Col2} = lists:unzip(
                     lists:map( fun(Line) -> lst2tup(
                                      lists:map(
                                        fun (X) -> fst(
                                                     string:to_integer(X)
                                                    ) end,
                                        string:lexemes(Line, " ")
                                       )
                                     ) end,
                       string:lexemes(
                         snd(file:read_file(
                               "./input.txt")
                            ), [$\n])
                      )
                    ),
    Lines = lists:zip(lists:sort(Col1), lists:sort(Col2)),
    Distances = lists:map(fun({Int1, Int2}) -> abs(Int1 - Int2) end, Lines),
    lists:sum(Distances).
