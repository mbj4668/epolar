-module(epolar_script).

-export([main/1]).

-include_lib("epolar/include/epolar.hrl").

main(Args) ->
    io:setopts([{encoding, unicode}]),
    eclip:parse(Args, spec(), #{}).

spec() ->
    #{
        require_cmd => true,
        cmds =>
            [
                cmd_convert(),
                cmd_print()
            ]
    }.

cmd_convert() ->
    #{
        cmd => "convert"
    }.

cmd_print() ->
    #{
        cmd => "print",
        args => [#{name => infname, metavar => "INFILE", type => file}],
        cb => fun print/3
    }.

print(_, _, FName) ->
    {ok, Sylk} = epolar_orc:read_sylk(FName),
    lists:foreach(
        fun({Cond, Polar}) ->
            io:format("~s\n", [Cond]),
            io:format("~*..=s\n", [byte_size(Cond), ""]),
            io:nl(),
            print_polar(Polar),
            io:nl()
        end,
        maps:to_list(Sylk)
    ).

print_polar(Polar) ->
    lists:foldl(
        fun(PTab, {TWS, First}) ->
            First orelse io:nl(),
            print_ptable(TWS, PTab),
            {TWS + 2, false}
        end,
        {6, true},
        tuple_to_list(Polar)
    ).

print_ptable(TWS, PTable) ->
    io:format("TWS: ~p kt\n\n", [TWS]),
    Data =
        [
            {"TWA", "BSP", "VMG", "AWS", "AWA", "Heel"}
            | [
                {fd(TWA), fk(BSP), fk(VMG), fk1(AWS), fd(AWA), fd(Heel)}
             || {TWA, _, #prow{bsp = BSP, vmg = VMG, aws = AWS, awa = AWA, heel = Heel}} <-
                    PTable
            ]
        ],
    io:put_chars(mtab:format(Data, #{style => simple, indent => "  "})).

fd(Deg) ->
    fx(Deg, 10, "°").

fk(Knots) ->
    fx(Knots, 100, "").

fk1(Knots) ->
    fx(Knots, 100, 1, "").

fx(V, Res, Unit) ->
    fx(V, Res, round(math:log10(Res)), Unit).

fx(V, Res, Dec, Unit) ->
    case (V div Res) * Res of
        V ->
            %% No decimals
            io_lib:format("~w~s", [V div Res, Unit]);
        _ ->
            io_lib:format("~.*f~s", [Dec, V / Res, Unit])
    end.
