-module(epolar).

%%% Library functions for polar tables.

-export([
    get_from_true_wind/3,
    get_optimal_beat_from_true_wind/2,
    get_optimal_run_from_true_wind/2
]).
-export([interpolate_prow/3]).

-export_type([speed/0, angle/0, ratio/0, polar/0, ptable/0]).

-include_lib("epolar/include/epolar.hrl").

%% knots, resolution 0.01
-type speed() :: integer().
%% degrees, resolution 0.1 (0-1800)
-type angle() :: integer().
%% 0-1, resolution 0.0001
-type ratio() :: integer().
-type optimal_twa() :: 'beat' | 'run' | 'undefined'.

%% A polar is a set of targets per a certain combination of TWS and
%% TWA.  It is represented as a tuple with entries for fixed TWS,
%% where each entry is a table of targets per TWA.
-type polar() ::
    {
        TWS_6 :: ptable(),
        TWS_8 :: ptable(),
        TWS_10 :: ptable(),
        TWS_12 :: ptable(),
        TWS_14 :: ptable(),
        TWS_16 :: ptable(),
        TWS_18 :: ptable(),
        TWS_20 :: ptable()
    }.

-type ptable() ::
    [{TWA :: angle(), Optimal :: optimal_twa(), #prow{}}].

%% Given a TWS and TWA, return a set of targets.
-spec get_from_true_wind(TWS :: speed(), TWA :: angle(), polar()) ->
    {ok, #prow{}}
    %% interpolated from too small twa
    | {min_twa, #prow{}}
    %% too low tws; return the lowest we know
    | {min_tws, #prow{}}
    %% too high tws; return the highest we know
    | {max_tws, #prow{}}
    %% too high twa; return the highest we know
    | {max_twa, #prow{}}.
get_from_true_wind(TWS, TWA, P) ->
    if
        TWS < 600 ->
            {_, PRow} = get_from_true_wind(600, TWA, P),
            {min_tws, PRow};
        TWS > 2000 ->
            {_, PRow} = get_from_true_wind(2000, TWA, P),
            {max_tws, PRow};
        TWS rem 200 == 0 ->
            %% exact TWS; cubic interpolation along TWA only
            Idx = (TWS div 200) - 2,
            PTab = element(Idx, P),
            get_prow_cubic(TWA, PTab);
        true ->
            %% bicubic: cubic interpolation in both TWS and TWA
            Idx = (TWS div 200) - 2,
            %% four ptabs for cubic TWS interpolation (clamped at boundaries)
            PTab0 = element(clamp(Idx - 1, 1, 8), P),
            PTab1 = element(Idx, P),
            PTab2 = element(Idx + 1, P),
            PTab3 = element(clamp(Idx + 2, 1, 8), P),
            %% cubic TWA interpolation in each ptab
            {_, PRow0} = get_prow_cubic(TWA, PTab0),
            {How, PRow1} = get_prow_cubic(TWA, PTab1),
            {_, PRow2} = get_prow_cubic(TWA, PTab2),
            {_, PRow3} = get_prow_cubic(TWA, PTab3),
            %% cubic TWS interpolation across the four results
            T = (TWS rem 200) / 200,
            {How, epolar_interp:prow_cubic(PRow0, PRow1, PRow2, PRow3, T)}
    end.

%% Given a TWS, return the best TWA for beating, and the associated
%% targets (the TWA that maximizes VMG).
-spec get_optimal_beat_from_true_wind(TWS :: speed(), polar()) ->
    {ok, {OptimalTWA :: angle(), #prow{}}}
    | {min_tws, {OptimalTWA :: angle(), #prow{}}}
    | {max_tws, {OptimalTWA :: angle(), #prow{}}}.
get_optimal_beat_from_true_wind(TWS, P) ->
    get_optimal_from_true_wind(TWS, beat, P).

%% Given a TWS, return the best TWA for running, and the associated
%% targets (the TWA that maximizes VMG).
-spec get_optimal_run_from_true_wind(TWS :: speed(), polar()) ->
    {ok, {OptimalTWA :: angle(), #prow{}}}
    | {min_tws, {OptimalTWA :: angle(), #prow{}}}
    | {max_tws, {OptimalTWA :: angle(), #prow{}}}.
get_optimal_run_from_true_wind(TWS, P) ->
    get_optimal_from_true_wind(TWS, run, P).

get_optimal_from_true_wind(TWS, Optimal, P) ->
    if
        TWS < 600 ->
            %% less than 3 m/s, do 3 m/s
            {ok, Res} = get_optimal_from_true_wind(600, Optimal, P),
            {min_tws, Res};
        TWS > 2000 ->
            %% more than 10 m/s, do 10 m/s
            {ok, Res} = get_optimal_from_true_wind(2000, Optimal, P),
            {max_tws, Res};
        TWS rem 200 == 0 ->
            %% ask for exact TWS; get its ptab
            Idx = (TWS div 200) - 2,
            PTab = element(Idx, P),
            {ok, get_optimal_prow(PTab, Optimal)};
        true ->
            %% find the closest two windspeeds
            %% e.g., for TWS 8.2 knots (820), find TWS 8 and 10, i.e.,
            %% idx 2 and 3.
            Idx = (TWS div 200) - 2,
            PTab0 = element(Idx, P),
            PTab1 = element(Idx + 1, P),
            {OptTWA0, PRow0} = get_optimal_prow(PTab0, Optimal),
            {OptTWA1, PRow1} = get_optimal_prow(PTab1, Optimal),
            Weight = (TWS rem 200) / 200,
            {ok, {
                epolar_interp:linear(OptTWA0, OptTWA1, Weight),
                interpolate_prow(PRow0, PRow1, Weight)
            }}
    end.

get_optimal_prow([{TWA, Optimal, PRow} | _], Optimal) ->
    {TWA, PRow};
get_optimal_prow([_ | T], Optimal) ->
    get_optimal_prow(T, Optimal).

get_prow_cubic(TWA, PTab) ->
    get_prow_cubic(TWA, none, PTab).

get_prow_cubic(TWA, none, [{PTWA, _, PRow1} | _]) when TWA < PTWA ->
    %% below the first TWA entry; linear interpolation from zero
    W = TWA / PTWA,
    PRow0 = #prow{
        bsp = 0,
        vmg = 0,
        aws = 0,
        awa = 0,
        heel = 0,
        reef = 1,
        flat = 1
    },
    {min_twa, epolar_interp:prow_linear(PRow0, PRow1, W)};
get_prow_cubic(TWA, Prev, [{PTWA0, _, PRow1}, {PTWA1, _, PRow2} | Rest]) when TWA =< PTWA1 ->
    %% found the interval; gather four control points for Catmull-Rom
    PRow0 =
        case Prev of
            none -> PRow1;
            {_, _, P} -> P
        end,
    PRow3 =
        case Rest of
            [{_, _, P2} | _] -> P2;
            [] -> PRow2
        end,
    T = (TWA - PTWA0) / (PTWA1 - PTWA0),
    {ok, epolar_interp:prow_cubic(PRow0, PRow1, PRow2, PRow3, T)};
get_prow_cubic(_TWA, _, [{_, _, PRow}]) ->
    {max_twa, PRow};
get_prow_cubic(TWA, _, [H | T]) ->
    get_prow_cubic(TWA, H, T).

interpolate_prow(PRow0, PRow1, W) ->
    epolar_interp:prow_linear(PRow0, PRow1, W).

clamp(V, Min, _Max) when V < Min -> Min;
clamp(V, _Min, Max) when V > Max -> Max;
clamp(V, _, _) -> V.
