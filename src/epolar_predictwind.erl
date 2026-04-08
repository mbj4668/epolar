-module(epolar_predictwind).
-export([write_file/2]).

-include_lib("epolar/include/epolar.hrl").

write_file(Polar, FName) ->
    {ok, Fd} = file:open(FName, [write]),
    PL = tuple_to_list(Polar),
    lists:foldl(
        fun(PTab, TWS) ->
            io:format(Fd, "~w\t0\t0", [TWS]),
            lists:foreach(
                fun({TWA, _OptimalTWA, #prow{bsp = BSP}}) ->
                    io:format(Fd, "\t~w\t~4.2f", [TWA div 10, BSP / 100])
                end,
                PTab
            ),
            io:format(Fd, "\n", []),
            TWS + 2
        end,
        6,
        PL
    ),
    file:close(Fd).
