-module(wind).

%% http://sailboatinstruments.blogspot.com/2011/05/true-wind-vmg-and-current-calculations.html
%% Arvel Gentry - Sailboat Performance Testing Techniques (1981)

-export([
    correct_awa_from_alignment_offset/2,
    correct_awa_from_upwash/5,
    correct_awa_from_heel/2,
    correct_heading_from_variation/2,
    leeway/3,
    speed_through_water/2,
    true_wind/5,
    vmg/3
]).
-export([rad_abs_to_rel/1, rad_rel_to_abs/1]).
-export([damped_true_wind_init/0, damped_true_wind_init/1, damped_true_wind/7]).
-export_type([damped_tw_state/0]).

%% resolution 1
-type knots() :: number().
%% resolution 1
-type rad() :: number().

%% relative to boat axis, port is negative

%% -?PI - ?PI
-type angle_rad_rel() :: angle(rad()).
%% 0 - 2*?PI
-type angle_rad_abs() :: angle(rad()).
%% any unit, any resolution.
-type angle(_UR) :: number().
%% any unit, any resolution.
-type speed(_UR) :: number().

-define(PI, 3.14159265).
-define(DEG_TO_RAD, (?PI / 180)).
-define(RAD_TO_DEG, (180 / ?PI)).
-define(MS_TO_KNOTS, (3600 / 1852)).
-define(KNOTS_TO_MS, (1852 / 3600)).

-spec correct_awa_from_alignment_offset(
    AWA :: angle_rad_abs(),
    Offset :: angle(rad())
) ->
    AWA :: angle_rad_abs().
correct_awa_from_alignment_offset(AWA, Offset) ->
    wrap_rel(AWA + Offset).

-spec correct_awa_from_upwash(
    AWA :: angle_rad_rel(),
    AWS :: knots(),
    %% S0 typically 2..3
    S0 :: number(),
    %% S1 typically 0 | 2..10
    S1 :: number(),
    %% Max AWS to consider, typically 25..30
    S2 :: angle_rad_rel()
) ->
    AWA :: angle_rad_rel().
%% Correct measured AWA for sail-induced upwash.
%%
%% Upwash increases the more close hauled you sail, is zero
%% on a beam reach, and is negative (downwash) when running.
%%
%% Upwash decreases with the wind speed.
%%
%% S0: increase to lower the effect of the wind speed (typically 2..3)
%% S1: multiplier; 0 means no upwash.  higher value increases the upwash. (typically 10..16)
%%     S1 is the "max" upwash.
%% S2: max AWS to consider
correct_awa_from_upwash(AWA, AWS, S0, S1, S2) ->
    AWSCapped = min(AWS, S2),
    AWA - (S1 * math:cos(S0 * AWSCapped * ?DEG_TO_RAD) * math:cos(AWA)) * ?DEG_TO_RAD.

-spec correct_awa_from_heel(
    AWA :: angle_rad_rel(),
    Heel :: angle_rad_rel()
) ->
    AWA :: angle_rad_rel().
correct_awa_from_heel(AWA, Heel) ->
    TanAWA = math:tan(AWA),
    CosHeel = math:cos(Heel),
    AWAHeel0 = math:atan(TanAWA / CosHeel),
    AWAHeel =
        if
            AWA > ?PI / 2 ->
                AWAHeel0 + ?PI;
            AWA < -?PI / 2 ->
                AWAHeel0 - ?PI;
            true ->
                AWAHeel0
        end,
    AWAHeel.

-spec correct_heading_from_variation(
    HDG :: angle(T),
    Variation :: angle(T)
) ->
    HDG :: angle(T).
correct_heading_from_variation(HDG, Variation) ->
    HDG + Variation.

-spec leeway(
    K :: number(),
    BoatSpeed :: knots(),
    Heel :: angle(T)
) ->
    angle(T).
%% Heel and Leeway are positive when the mast leans to starboard (port
%% tack) and negative when the mast leans to port (starboard tack).
leeway(K, BSP, Heel) ->
    if
        BSP < 1 ->
            0;
        true ->
            Leeway = K * Heel / (BSP * BSP),
            Leeway
    end.

-spec speed_through_water(Leeway :: rad(), BSP :: speed(T)) ->
    speed(T).
%% BSP is "raw" boat speed, from the sensor
%% STW is Speed Through Water (with Leeway taken into account)
speed_through_water(Leeway, BSP) ->
    STW = BSP / math:cos(Leeway),
    STW.

-spec true_wind(
    AWA :: angle_rad_rel(),
    AWS :: speed(T),
    BSP :: speed(T),
    Leeway :: rad(),
    Heading :: rad()
) ->
    {TWA :: angle_rad_rel(), TWS :: speed(T), TWD :: rad()}.
%% Calculate true wind angle, speed and direction (TWA, TWS, TWD).
%% If Heading is not known, set it to 0.  TWD will not be correct.
%% AWA and TWA are relative to boat axis, negative on port.
true_wind(AWA, AWS, BSP, Leeway, HDG) ->
    STW = speed_through_water(Leeway, BSP),
    %% Cartesian AWA
    CAWA = 3 * ?PI / 2 - AWA,
    %% Calculate component of boat speed perpendicular to boat axis
    LateralSpeed = STW * math:sin(Leeway),
    AWS_X = AWS * math:cos(CAWA),
    AWS_Y = AWS * math:sin(CAWA),
    TWS_X = AWS_X + LateralSpeed,
    TWS_Y = AWS_Y + BSP,
    TWS = math:sqrt(TWS_X * TWS_X + TWS_Y * TWS_Y),
    CTWA = math:atan2(TWS_Y, TWS_X),
    TWA0 = 3 * ?PI / 2 - CTWA,
    TWA1 =
        if
            AWA >= 0 ->
                math:fmod(TWA0, 2 * ?PI);
            true ->
                TWA0 - 2 * ?PI
        end,
    TWA = wrap_rel(TWA1),
    %% Calculate True Wind Direction
    TWD0 = HDG + TWA,
    TWD = wrap_abs(TWD0),
    {TWA, TWS, TWD}.

-spec vmg(STW :: speed(T), TWA0 :: rad(), Leeway :: rad()) ->
    speed(T).
vmg(STW, TWA0, Leeway) ->
    TWA =
        if
            TWA0 > ?PI ->
                TWA0 - 2 * ?PI;
            true ->
                TWA0
        end,
    VMG = STW * math:cos(-TWA + Leeway),
    VMG.

%%%===================================================================
%%% Damped true wind
%%%===================================================================
%%
%% Computes TWA / TWS / TWD damped in the ground frame.
%%
%% Why ground frame: TWA and TWS are boat-relative and change every
%% time the boat yaws, so averaging them scalar-wise smears through
%% tacks. TWD is earth-relative, stable across yaw, and the natural
%% quantity to filter. We keep the filter state as the (N, E) vector
%% components of the true wind in the ground frame. On each sample
%% we compute an instantaneous TW vector, mix it into the filtered
%% vector with a time-constant `Tau`, and derive consistent TWD/TWS
%% from the filtered vector. TWA is then TWD minus the *current*
%% heading, so tacks snap instantly while wind shifts stay smoothed.

-record(dtw_st, {
    tau = 10.0 :: float(),
    n = undefined :: float() | undefined,
    e = undefined :: float() | undefined,
    last_t = undefined :: float() | undefined
}).
-opaque damped_tw_state() :: #dtw_st{}.

-spec damped_true_wind_init() -> damped_tw_state().
damped_true_wind_init() -> damped_true_wind_init(10.0).

-spec damped_true_wind_init(Tau :: number()) -> damped_tw_state().
damped_true_wind_init(Tau) -> #dtw_st{tau = float(Tau)}.

-define(r(R), R * ?RAD_TO_DEG).

-spec damped_true_wind(
    TimeMs :: integer(),
    AWA :: angle_rad_rel(),
    AWS :: speed(T),
    BSP :: speed(T),
    HDG :: angle_rad_abs(),
    Leeway :: rad(),
    damped_tw_state()
) ->
    {TWA :: angle_rad_rel(), TWS :: speed(T), TWD :: angle_rad_abs(), damped_tw_state()}.
%% AWS and BSP must be in the same unit; TWS is returned in that unit.
damped_true_wind(TimeMs, AWA, AWS, BSP, HDG, Leeway, S) ->
    T = TimeMs / 1000.0,
    {_TWAi, TWSi, TWDi} = true_wind(AWA, AWS, BSP, Leeway, HDG),
    %% Instantaneous TW vector in the ground frame. The choice of
    %% axes (N=cos, E=sin vs any other orthogonal pair) doesn't
    %% matter so long as it is consistent - we only use it to LPF.
    Ni = TWSi * math:cos(TWDi),
    Ei = TWSi * math:sin(TWDi),
    {Nf, Ef, Last} = dtw_lpf(Ni, Ei, T, S),
    TWS = math:sqrt(Nf * Nf + Ef * Ef),
    TWD0 = math:atan2(Ef, Nf),
    TWD =
        if
            TWD0 < 0 -> TWD0 + 2 * ?PI;
            true -> TWD0
        end,
    TWA = wrap_rel(TWD - HDG),
    S1 = S#dtw_st{n = Nf, e = Ef, last_t = Last},
    io:format(
        "TWAi: ~.2f, TWA: ~.2f  TWDi: ~.2f TWD ~.2f\n",
        [?r(_TWAi), ?r(TWA), ?r(TWDi), ?r(TWD)]
    ),
    {TWA, TWS, TWD, S1}.

dtw_lpf(N, E, T, #dtw_st{n = undefined}) ->
    {N, E, T};
dtw_lpf(N, E, T, #dtw_st{n = N0, e = E0, last_t = Lt, tau = Tau}) ->
    Dt =
        case Lt of
            undefined -> 0.0;
            _ -> max(0.0, T - Lt)
        end,
    A = 1.0 - math:exp(-Dt / Tau),
    {N0 + A * (N - N0), E0 + A * (E - E0), T}.

wrap_rel(A) when A > ?PI -> wrap_rel(A - 2 * ?PI);
wrap_rel(A) when A < -?PI -> wrap_rel(A + 2 * ?PI);
wrap_rel(A) -> A.

wrap_abs(A) when A > 2 * ?PI -> wrap_abs(A - 2 * ?PI);
wrap_abs(A) when A < 0 -> wrap_abs(A + 2 * ?PI);
wrap_abs(A) -> A.

-spec rad_abs_to_rel(angle_rad_abs()) -> angle_rad_rel().
rad_abs_to_rel(Angle) ->
    if
        Angle > ?PI ->
            Angle - 2 * ?PI;
        true ->
            Angle
    end.

-spec rad_rel_to_abs(angle_rad_rel()) -> angle_rad_abs().
rad_rel_to_abs(Angle) ->
    if
        Angle < 0 ->
            2 * ?PI + Angle;
        true ->
            Angle
    end.
