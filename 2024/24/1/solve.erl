-module(solve).
-export([main/0]).

sort_fun({Key1, _}, {Key2, _}) -> Key1 =< Key2.

init_registers(Lines) -> init_registers(Lines, maps:new()).
init_registers([Head | Tail], Map) ->
    case lists:member($:, Head) of
        true ->
            init_registers(
                Tail,
                maps:put(lists:takewhile(fun(C) -> C /= $: end, Head), char2bool(lists:last(Head)), Map)
            );
        false ->
            {Map, [Head | Tail]}
    end.


execute([], Registers) ->
    Registers;
execute([[Reg1, Op, Reg2, Dest] | Tail], Registers) ->
    case maps:is_key(Reg1, Registers) and maps:is_key(Reg2, Registers) of
        true ->
            Val1 = maps:get(Reg1, Registers),
            Val2 = maps:get(Reg2, Registers),
            NewVal =
                case Op of
                    "AND" -> Val1 and Val2;
                    "XOR" -> Val1 xor Val2;
                    "OR" -> Val1 or Val2
                end,
            execute(Tail, maps:put(Dest, NewVal, Registers));
        false ->
            execute(Tail ++ [[Reg1, Op, Reg2, Dest]], Registers)
    end.

parse_op(Line) -> string:lexemes(Line -- "->", [$\s]).

zfun([$z | _], _) -> true;
zfun(_, _) -> false.

char2bool($1) -> true;
char2bool($0) -> false.

bool2int(true) -> 1;
bool2int(false) -> 0.

print_z(Registers) ->
    ZRegs = maps:to_list(maps:filter(fun zfun/2, Registers)),
    {_, Bools} = lists:unzip(lists:sort(fun sort_fun/2, ZRegs)),
    lists:sum(
        lists:zipwith(
            fun(Bool, Shift) -> (1 bsl Shift) * bool2int(Bool) end,
            Bools,
            lists:seq(0, length(Bools) - 1)
        )
    ).

main() ->
    Lines = string:lexemes(lists:flatten(help:stdin()), [$\n]),
    {Registers, Program} = init_registers(Lines),
    ParsedProgram = lists:map(fun parse_op/1, Program),
    ExecutedRegisters = execute(ParsedProgram, Registers),
    io:write(print_z(ExecutedRegisters)),
    io:nl(),
    halt().
