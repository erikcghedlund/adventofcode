-module(solve).
-export([main/0]).




main() -> 
    io:format("~p\n", [solve()]),
    halt().
solve() ->
    {Col1, Col2} = lists:unzip(
                     lists:map( fun(Line) -> help:lst2tup(
                                      lists:map(
                                        fun (X) -> help:fst(
                                                     string:to_integer(X)
                                                    ) end,
                                        string:lexemes(Line, " ")
                                       )
                                     ) end,
                       string:lexemes(
                         help:stdin(),
                         [$\n])
                      )
                    ),
    Distances = lists:map(fun(Int) -> Int * help:count(Int, Col2) end, Col1),
    lists:sum(Distances).
