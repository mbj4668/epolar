-module(epolar_interp_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epolar/include/epolar.hrl").

%%% ---------------------------------------------------------------
%%% linear/3
%%% ---------------------------------------------------------------

linear_at_zero_test() ->
    ?assertEqual(100, epolar_interp:linear(100, 200, 0)).

linear_at_one_test() ->
    ?assertEqual(200, epolar_interp:linear(100, 200, 1)).

linear_midpoint_test() ->
    ?assertEqual(150, epolar_interp:linear(100, 200, 0.5)).

linear_quarter_test() ->
    ?assertEqual(125, epolar_interp:linear(100, 200, 0.25)).

linear_negative_values_test() ->
    ?assertEqual(0, epolar_interp:linear(-100, 100, 0.5)).

linear_equal_values_test() ->
    ?assertEqual(42, epolar_interp:linear(42, 42, 0.7)).

linear_undefined_test() ->
    ?assertEqual(undefined, epolar_interp:linear(undefined, undefined, 0.5)).

%%% ---------------------------------------------------------------
%%% catmull_rom/5
%%% ---------------------------------------------------------------

%% At T=0 the result must equal P1.
catmull_rom_at_zero_test() ->
    ?assertEqual(200, epolar_interp:catmull_rom(100, 200, 300, 400, 0)).

%% At T=1 the result must equal P2.
catmull_rom_at_one_test() ->
    ?assertEqual(300, epolar_interp:catmull_rom(100, 200, 300, 400, 1)).

%% For equally-spaced linear data the cubic must reproduce the line.
catmull_rom_linear_data_test() ->
    ?assertEqual(250, epolar_interp:catmull_rom(100, 200, 300, 400, 0.5)).

%% Midpoint of a curve (1, 4, 9, 16) — i.e. x^2 at x = 2, 3, 4, 5.
%% Expected between P1=400 and P2=900 at T=0.5:
%%   0.5*(2*400 + (-100+900)*0.5 + (200-2000+3600-1600)*0.25
%%        + (-100+1200-2700+1600)*0.125)
%%   = 0.5*(800 + 400 + 50 + 0) = 625
catmull_rom_quadratic_test() ->
    ?assertEqual(625, epolar_interp:catmull_rom(100, 400, 900, 1600, 0.5)).

catmull_rom_undefined_test() ->
    ?assertEqual(undefined,
                 epolar_interp:catmull_rom(undefined, undefined,
                                           undefined, undefined, 0.5)).

%% When outer points equal the inner points (clamped boundaries),
%% the result at T=0 and T=1 must still be exact.
catmull_rom_clamped_boundaries_test() ->
    ?assertEqual(200, epolar_interp:catmull_rom(200, 200, 300, 300, 0)),
    ?assertEqual(300, epolar_interp:catmull_rom(200, 200, 300, 300, 1)).

%%% ---------------------------------------------------------------
%%% prow_linear/3
%%% ---------------------------------------------------------------

mk_prow(BSP, VMG) ->
    #prow{bsp = BSP, vmg = VMG, aws = 0, awa = 0,
          heel = 0, reef = 10000, flat = 10000}.

prow_linear_midpoint_test() ->
    P0 = mk_prow(100, 200),
    P1 = mk_prow(300, 400),
    R  = epolar_interp:prow_linear(P0, P1, 0.5),
    ?assertEqual(200, R#prow.bsp),
    ?assertEqual(300, R#prow.vmg).

prow_linear_at_zero_test() ->
    P0 = mk_prow(100, 200),
    P1 = mk_prow(300, 400),
    ?assertEqual(P0, epolar_interp:prow_linear(P0, P1, 0)).

prow_linear_at_one_test() ->
    P0 = mk_prow(100, 200),
    P1 = mk_prow(300, 400),
    ?assertEqual(P1, epolar_interp:prow_linear(P0, P1, 1)).

prow_linear_undefined_fields_test() ->
    P0 = #prow{bsp = 100, vmg = undefined, aws = 0, awa = 0,
               heel = 0, reef = 10000, flat = 10000},
    P1 = #prow{bsp = 200, vmg = undefined, aws = 0, awa = 0,
               heel = 0, reef = 10000, flat = 10000},
    R = epolar_interp:prow_linear(P0, P1, 0.5),
    ?assertEqual(150, R#prow.bsp),
    ?assertEqual(undefined, R#prow.vmg).

%%% ---------------------------------------------------------------
%%% prow_cubic/5
%%% ---------------------------------------------------------------

prow_cubic_at_endpoints_test() ->
    P0 = mk_prow(100, 100),
    P1 = mk_prow(200, 200),
    P2 = mk_prow(300, 300),
    P3 = mk_prow(400, 400),
    ?assertEqual(P1, epolar_interp:prow_cubic(P0, P1, P2, P3, 0)),
    ?assertEqual(P2, epolar_interp:prow_cubic(P0, P1, P2, P3, 1)).

prow_cubic_linear_data_test() ->
    P0 = mk_prow(100, 100),
    P1 = mk_prow(200, 200),
    P2 = mk_prow(300, 300),
    P3 = mk_prow(400, 400),
    R = epolar_interp:prow_cubic(P0, P1, P2, P3, 0.5),
    ?assertEqual(250, R#prow.bsp),
    ?assertEqual(250, R#prow.vmg).

prow_cubic_all_fields_test() ->
    P0 = #prow{bsp = 100, vmg = 100, aws = 100, awa = 100,
               heel = 100, reef = 100, flat = 100},
    P1 = #prow{bsp = 200, vmg = 200, aws = 200, awa = 200,
               heel = 200, reef = 200, flat = 200},
    P2 = #prow{bsp = 300, vmg = 300, aws = 300, awa = 300,
               heel = 300, reef = 300, flat = 300},
    P3 = #prow{bsp = 400, vmg = 400, aws = 400, awa = 400,
               heel = 400, reef = 400, flat = 400},
    R = epolar_interp:prow_cubic(P0, P1, P2, P3, 0.5),
    ?assertEqual(250, R#prow.bsp),
    ?assertEqual(250, R#prow.vmg),
    ?assertEqual(250, R#prow.aws),
    ?assertEqual(250, R#prow.awa),
    ?assertEqual(250, R#prow.heel),
    ?assertEqual(250, R#prow.reef),
    ?assertEqual(250, R#prow.flat).

prow_cubic_undefined_fields_test() ->
    P0 = #prow{bsp = 100, vmg = undefined, aws = 0, awa = 0,
               heel = 0, reef = 10000, flat = 10000},
    P1 = #prow{bsp = 200, vmg = undefined, aws = 0, awa = 0,
               heel = 0, reef = 10000, flat = 10000},
    P2 = #prow{bsp = 300, vmg = undefined, aws = 0, awa = 0,
               heel = 0, reef = 10000, flat = 10000},
    P3 = #prow{bsp = 400, vmg = undefined, aws = 0, awa = 0,
               heel = 0, reef = 10000, flat = 10000},
    R = epolar_interp:prow_cubic(P0, P1, P2, P3, 0.5),
    ?assertEqual(250, R#prow.bsp),
    ?assertEqual(undefined, R#prow.vmg).

%% Verify cubic differs from linear on non-linear data.
prow_cubic_vs_linear_test() ->
    P0 = mk_prow(100, 0),
    P1 = mk_prow(400, 0),
    P2 = mk_prow(900, 0),
    P3 = mk_prow(1600, 0),
    Cubic  = epolar_interp:prow_cubic(P0, P1, P2, P3, 0.5),
    Linear = epolar_interp:prow_linear(P1, P2, 0.5),
    %% Cubic should give 625 (exact for x^2), linear gives 650.
    ?assertEqual(625, Cubic#prow.bsp),
    ?assertEqual(650, Linear#prow.bsp).
