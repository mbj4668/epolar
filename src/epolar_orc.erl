-module(epolar_orc).
-export([read_sylk/1]).

-include_lib("epolar/include/epolar.hrl").

-spec read_sylk(FName :: string()) ->
        {ok, #{Sail :: binary() => epolar:polar()}}
      | {error, term()}.
%% Read a ORC SYLK (.slk) file and return a map of polars,
%% one for each "Sail" configuration in the input file.
%% The "best" sail configuration is called <<"BestPerf">>.
read_sylk(FName) ->
    case file:read_file(FName) of
        {ok, Data} ->
            [IdLine | Lines] =
                case binary:split(Data, <<"\r\n">>, [global, trim]) of
                    [_] ->
                        binary:split(Data, <<"\n">>, [global, trim]);
                    Res ->
                        Res
                end,
            case IdLine of
                <<"ID;PWXL;N;E">> -> % this is the format we know
                    sylk_header([parse_line(Line) || Line <- Lines]);
                _ ->
                    {error, unknown_id}
            end;
        Error ->
            Error
    end.

%% Each line is one cell, on the form: C;Yn;Xm;Kval
%% where n,m are integers (column, row), and val is
%% a double qouted string, an integer or a float.
%% Ret: {Y, X, K} | 'end' | 'eof'
parse_line(Line) ->
    case binary:split(Line, <<";">>, [global]) of
        [<<"C">>, <<"Y", Y/binary>>, <<"X",X/binary>>, <<"K",K/binary>>] ->
            {binary_to_integer(Y), binary_to_integer(X), parse_k(K)};
        [<<"E">>] ->
            'end';
        [<<>>] ->
            'eof'
    end.

parse_k(<<"\"", Rest/binary>>) ->
    Len = byte_size(Rest) - 1,
    <<Str:Len/binary, "\"">> = Rest,
    Str;
parse_k(K) ->
    try
        binary_to_integer(K)
    catch
        _:_ ->
            try
                binary_to_float(K)
            catch
                _:_ ->
                    {error, {unknown_k, K}}
            end
    end.

get_row([{Y, 1, Val} | Lines], v1) ->
    get_raw_row(Lines, Y, 2, [Val]);
get_row([{Y, 1, Val}, {_, 2, _} | Lines], v2023) ->
    %% skip (new in 2023 format) column 2 (Id of sail)
    get_raw_row(Lines, Y, 3, [Val]);
get_row(['end'], _) ->
    {'end', []}.

%% ensure the first column (X value) is 1
get_raw_row([{Y, 1, Val} | Lines]) ->
    get_raw_row(Lines, Y, 2, [Val]);
get_raw_row(['end']) ->
    {'end', []}.

%% [the match on X is an assertion
get_raw_row([{Y, X, Val} | T], Y, X, Acc) ->
    get_raw_row(T, Y, X+1, [Val | Acc]);
get_raw_row(Lines, _, _, Acc) ->
    {lists:reverse(Acc), Lines}.

sylk_header(Lines0) ->
    {Header, Lines1} = get_raw_row(Lines0),
    case Header of
        [<<"Sail">>, <<"Id">>, <<"TWS">>, <<"Condition">>, <<"TWA">>, <<"BTV">>,
         <<"VMG">>, <<"AWS">>, <<"AWA">>, <<"Heel">>, <<"Reef">>, <<"Flat">>] ->
            %% 2023 format
            sylk_rows(Lines1, #{}, v2023);
        [<<"Sail">>, <<"TWS">>, <<"Condition">>, <<"TWA">>, <<"BTV">>,
         <<"VMG">>, <<"AWS">>, <<"AWA">>, <<"Heel">>, <<"Reef">>, <<"Flat">>] ->
            %% the format we understand!
            sylk_rows(Lines1, #{}, v1);
        _ ->
            {error, {unknown_header, Header}}
    end.

sylk_rows(Lines0, M, Vsn) ->
    case get_row(Lines0, Vsn) of
        {[Sail, TWS, Condition, TWA, BSP, VMG, AWS, AWA, Heel, Reef, Flat],
         Lines1} ->
            Polar1 =
                case maps:find(Sail, M) of
                    {ok, Polar0} ->
                        Polar0;
                    _ ->
                        mk_empty_polar()
                end,
            Idx = (TWS - 4) div 2,
            PTable0 = element(Idx, Polar1),
            PRow = #prow{bsp = speed(BSP), vmg = speed(VMG),
                         aws = speed(AWS), awa = angle(AWA),
                         heel = angle(Heel), reef = ratio(Reef),
                         flat = ratio(Flat)},
            Optimal =
                case Condition of
                    <<"beat">> -> beat;
                    <<"run">> -> run;
                    <<"reach">> -> undefined
                end,
            PTable1 = lists:keymerge(1, [{angle(TWA), Optimal, PRow}], PTable0),
            Polar2 = setelement(Idx, Polar1, PTable1),
            sylk_rows(Lines1, M#{Sail => Polar2}, Vsn);
        {'end', []} ->
            {ok, patch_polars(M)}
    end.

mk_empty_polar() ->
    {[], [], [], [], [], [], [], []}.

speed(F) ->
    round(F * 100).

angle(F) ->
    round(F * 10).

ratio(F) ->
    round(F * 10000).

%% we create an interpolated table for TWS = 18 (Idx = 7), since that is not
%% included in the polar file
patch_polars(M) ->
    maps:map(fun(_, P) -> patch_polar(P) end, M).

patch_polar(P) ->
    case element(7, P) of
        [] ->
            setelement(7, P, interpolate_ptable(element(6, P), element(8, P)));
        _ ->
            P
    end.

interpolate_ptable([{TWA0, Optimal, PRow0} | T0],
                   [{TWA1, Optimal, PRow1} | T1]) ->
    [{avg(TWA0, TWA1), Optimal, epolar:interpolate_prow(PRow0, PRow1, 0.5)} |
     interpolate_ptable(T0, T1)];
interpolate_ptable([], []) ->
    [].

avg(A, B) ->
    round((A + B) / 2).

