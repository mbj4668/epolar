-module(epolar_sailgrib).
-export([write_file/2]).

-include_lib("epolar/include/epolar.hrl").

write_file(Polar, FName) ->
    TWSL = [6, 8, 10, 12, 14, 16, 18, 20],
    TWAL = [40, 45, 50, 60, 70, 75, 80, 90, 110, 120, 135, 140, 150, 165, 180],
    {ok, Fd} = file:open(FName, [write]),

    io:format(Fd, "TWA\TWS\t0\t", []),
    lists:foreach(fun(TWS) -> io:format(Fd, "~w\t", [TWS]) end, TWSL),
    io:format(Fd, "\t\r\n", []),

    io:format(Fd, "0\t0.0\t", []),
    lists:foreach(fun(_TWS) -> io:format(Fd, "0.0\t", []) end, TWSL),
    io:format(Fd, "\t\r\n", []),

    lists:foreach(
        fun(TWA) ->
            io:format(Fd, "~w\t0.0\t", [TWA]),
            lists:foreach(
                fun(TWS) ->
                    {_, PRow} = epolar:get_from_true_wind(TWS * 100, TWA * 10, Polar),
                    io:format(Fd, "~.1f\t", [PRow#prow.bsp / 100])
                end,
                TWSL
            ),
            io:format(Fd, "\t\r\n", [])
        end,
        TWAL
    ),
    file:close(Fd).
