-module(polar).

%%% Library functions for polar tables.

-export([get_from_true_wind/3,
         get_optimal_beat_from_true_wind/2,
         get_optimal_run_from_true_wind/2]).
-export([interpolate_prow/3]).

-export_type([speed/0, angle/0, ratio/0, polar/0, ptable/0]).

-include_lib("epolar/include/polar.hrl").

-type speed() :: integer(). % knots, resolution 0.01
-type angle() :: integer(). % degrees, resolution 0.1 (0-1800)
-type ratio() :: integer(). % 0-1, resolution 0.0001
-type optimal_twa() :: 'beat' | 'run' | 'undefined'.

%% A polar is a set of targets per a certain combination of TWS and
%% TWA.  It is represented as a tuple with entries for fixed TWS,
%% where each entry is a table of targets per TWA.
-type polar() ::
        {TWS_6 :: ptable(),
         TWS_8 :: ptable(),
         TWS_10 :: ptable(),
         TWS_12 :: ptable(),
         TWS_14 :: ptable(),
         TWS_16 :: ptable(),
         TWS_18 :: ptable(),
         TWS_20 :: ptable()}.

-type ptable() ::
        [{TWA :: angle(), Optimal :: optimal_twa(), #prow{}}].

%% Given a TWS and TWA, return a set of targets.
-spec get_from_true_wind(TWS :: speed(), TWA :: angle(), polar()) ->
        {ok, #prow{}}
      | {min_twa, #prow{}}  % interpolated from too small twa
      | {min_tws, #prow{}}  % too low tws; return the lowest we know
      | {max_tws, #prow{}}. % too high tws; return the highest we know
get_from_true_wind(TWS, TWA, P) ->
    if TWS < 600 ->
            {_, PRow} = get_from_true_wind(600, TWA, P),
            {min_tws, PRow};
       TWS > 2000 ->
            {_, PRow} = get_from_true_wind(2000, TWA, P),
            {max_tws, PRow};
       TWS rem 200 == 0 ->
            %% ask for exact TWS; get its ptab
            Idx = (TWS div 200) - 2,
            PTab = element(Idx, P),
            %% then find or interpolate a prow for the given TWA in this ptab
            get_prow(TWA, PTab);
       true ->
            %% find the closest two windspeeds
            %% e.g., for TWS 8.2 knots (820), find TWS 8 and 10, i.e.,
            %% idx 2 and 3.
            Idx = (TWS div 200) - 2,
            PTab0 = element(Idx, P),
            PTab1 = element(Idx+1, P),
            %% in each of these ptabs, find or interpolate a prow for the
            %% given TWA.
            {How, PRow0} = get_prow(TWA, PTab0),
            %% we ignore the How for the next TWA; if it was min_twa for the
            %% first point we return min_twa for the result.  this is
            %% questionable (FIXME).
            {_How, PRow1} = get_prow(TWA, PTab1),
            %% then interpolate based on the given TWS
            {How, interpolate_prow(PRow0, PRow1, (TWS rem 200) / 200)}
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
    if TWS < 600 ->
            {ok, Res} = get_optimal_from_true_wind(600, Optimal, P),
            {min_tws, Res};
       TWS > 2000 ->
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
            PTab1 = element(Idx+1, P),
            {OptTWA0, PRow0} = get_optimal_prow(PTab0, Optimal),
            {OptTWA1, PRow1} = get_optimal_prow(PTab1, Optimal),
            Weight = (TWS rem 200) / 200,
            {ok, {avg(OptTWA0, OptTWA1, Weight),
                  interpolate_prow(PRow0, PRow1, Weight)}}
    end.

get_optimal_prow([{TWA, Optimal, PRow} | _], Optimal) ->
    {TWA, PRow};
get_optimal_prow([_ | T], Optimal) ->
    get_optimal_prow(T, Optimal).


get_prow(TWA, [{PTWA, _, PRow1} | _]) when TWA < PTWA ->
    %% we handle this by interpolating from a made up "0" TWA.
    %% this is probably not correct (FIXME).
    W = TWA / PTWA,
    PRow0 = #prow{bsp = 0, vmg = 0, aws = 0, awa = 0,
                  heel = 0, reef = 1, flat = 1},
    {min_twa, interpolate_prow(PRow0, PRow1, W)};
get_prow(TWA, [{PTWA0, _, PRow0}, {PTWA1, _, PRow1} | _])
  when TWA =< PTWA1 ->
    W = (TWA - PTWA0) / (PTWA1 - PTWA0),
    {ok, interpolate_prow(PRow0, PRow1, W)};
get_prow(TWA, [_ | T]) ->
    get_prow(TWA, T).

interpolate_prow(#prow{bsp = BSP0, vmg = VMG0, aws = AWS0, awa = AWA0,
                       heel = Heel0, reef = Reef0, flat = Flat0},
                 #prow{bsp = BSP1, vmg = VMG1, aws = AWS1, awa = AWA1,
                       heel = Heel1, reef = Reef1, flat = Flat1},
                 W) ->
    #prow{bsp = avg(BSP0, BSP1, W), vmg = avg(VMG0, VMG1, W),
          aws = avg(AWS0, AWS1, W), awa = avg(AWA0, AWA1, W),
          heel = avg(Heel0, Heel1, W), reef = avg(Reef0, Reef1, W),
          flat = avg(Flat0, Flat1, W)}.

avg(A, B, Weight) when is_integer(A), is_integer(B), Weight >= 0, Weight =< 1 ->
    round(A * (1 - Weight) + B * Weight);
avg(undefined, undefined, _) ->
    undefined.
