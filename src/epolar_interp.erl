-module(epolar_interp).

%%% Interpolation methods for polar table data.

-export([linear/3, catmull_rom/5]).
-export([prow_linear/3, prow_cubic/5]).

-include_lib("epolar/include/epolar.hrl").

%% Linear interpolation between two values.
%% T is in [0, 1].
-spec linear
    (integer(), integer(), number()) -> integer();
    (undefined, undefined, number()) -> undefined.
linear(A, B, T) when is_integer(A), is_integer(B), T >= 0, T =< 1 ->
    round(A * (1 - T) + B * T);
linear(undefined, undefined, _) ->
    undefined.

%% Catmull-Rom cubic spline interpolation.
%% Given four control points P0, P1, P2, P3, interpolates between
%% P1 and P2.  T is in [0, 1].
-spec catmull_rom
    (integer(), integer(), integer(), integer(), number()) ->
        integer();
    (undefined, undefined, undefined, undefined, number()) ->
        undefined.
catmull_rom(P0, P1, P2, P3, T) when
    is_integer(P0), is_integer(P1), is_integer(P2), is_integer(P3)
->
    T2 = T * T,
    T3 = T2 * T,
    round(
        0.5 *
            ((2 * P1) +
                (-P0 + P2) * T +
                (2 * P0 - 5 * P1 + 4 * P2 - P3) * T2 +
                (-P0 + 3 * P1 - 3 * P2 + P3) * T3)
    );
catmull_rom(undefined, undefined, undefined, undefined, _) ->
    undefined.

%% Linear interpolation of a prow.
-spec prow_linear(#prow{}, #prow{}, number()) -> #prow{}.
prow_linear(
    #prow{
        bsp = BSP0,
        vmg = VMG0,
        aws = AWS0,
        awa = AWA0,
        heel = Heel0,
        reef = Reef0,
        flat = Flat0
    },
    #prow{
        bsp = BSP1,
        vmg = VMG1,
        aws = AWS1,
        awa = AWA1,
        heel = Heel1,
        reef = Reef1,
        flat = Flat1
    },
    T
) ->
    #prow{
        bsp = linear(BSP0, BSP1, T),
        vmg = linear(VMG0, VMG1, T),
        aws = linear(AWS0, AWS1, T),
        awa = linear(AWA0, AWA1, T),
        heel = linear(Heel0, Heel1, T),
        reef = linear(Reef0, Reef1, T),
        flat = linear(Flat0, Flat1, T)
    }.

%% Catmull-Rom cubic interpolation of a prow.
%% Interpolates between PRow1 and PRow2; PRow0 and PRow3 are the
%% outer control points.
-spec prow_cubic(#prow{}, #prow{}, #prow{}, #prow{}, number()) -> #prow{}.
prow_cubic(
    #prow{
        bsp = BSP0,
        vmg = VMG0,
        aws = AWS0,
        awa = AWA0,
        heel = Heel0,
        reef = Reef0,
        flat = Flat0
    },
    #prow{
        bsp = BSP1,
        vmg = VMG1,
        aws = AWS1,
        awa = AWA1,
        heel = Heel1,
        reef = Reef1,
        flat = Flat1
    },
    #prow{
        bsp = BSP2,
        vmg = VMG2,
        aws = AWS2,
        awa = AWA2,
        heel = Heel2,
        reef = Reef2,
        flat = Flat2
    },
    #prow{
        bsp = BSP3,
        vmg = VMG3,
        aws = AWS3,
        awa = AWA3,
        heel = Heel3,
        reef = Reef3,
        flat = Flat3
    },
    T
) ->
    #prow{
        bsp = catmull_rom(BSP0, BSP1, BSP2, BSP3, T),
        vmg = catmull_rom(VMG0, VMG1, VMG2, VMG3, T),
        aws = catmull_rom(AWS0, AWS1, AWS2, AWS3, T),
        awa = catmull_rom(AWA0, AWA1, AWA2, AWA3, T),
        heel = catmull_rom(Heel0, Heel1, Heel2, Heel3, T),
        reef = catmull_rom(Reef0, Reef1, Reef2, Reef3, T),
        flat = catmull_rom(Flat0, Flat1, Flat2, Flat3, T)
    }.
