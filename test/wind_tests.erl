-module(wind_tests).
-include_lib("eunit/include/eunit.hrl").

-define(PI, 3.14159265).
-define(EPS, 1.0e-6).
-define(assertClose(A, B), ?assert(abs((A) - (B)) < ?EPS)).
-define(assertClose(A, B, Eps), ?assert(abs((A) - (B)) < (Eps))).

%%% ---------------------------------------------------------------
%%% correct_awa_from_alignment_offset/2
%%% ---------------------------------------------------------------

alignment_offset_zero_test() ->
    ?assertClose(0.5, wind:correct_awa_from_alignment_offset(0.5, 0.0)).

alignment_offset_positive_test() ->
    ?assertClose(0.6, wind:correct_awa_from_alignment_offset(0.5, 0.1)).

alignment_offset_negative_test() ->
    ?assertClose(0.4, wind:correct_awa_from_alignment_offset(0.5, -0.1)).

%% Offset that pushes past +pi should wrap into the negative half.
alignment_offset_wraps_test() ->
    R = wind:correct_awa_from_alignment_offset(?PI - 0.05, 0.1),
    ?assertClose(R, -?PI + 0.05, 1.0e-5).

%%% ---------------------------------------------------------------
%%% correct_awa_from_upwash/5
%%% ---------------------------------------------------------------

%% S1 = 0 disables the correction entirely.
upwash_zero_gain_test() ->
    ?assertClose(0.3, wind:correct_awa_from_upwash(0.3, 12.0, 0.1, 0.0, 20.0)).

%% Known-value check: AWA - S1*cos(S0*AWS)*cos(AWA).
upwash_value_test() ->
    AWA = 0.4, AWS = 10.0, S0 = 0.1, S1 = 0.05, S2 = 20.0,
    Expected = AWA - S1 * math:cos(S0 * AWS) * math:cos(AWA),
    ?assertClose(Expected, wind:correct_awa_from_upwash(AWA, AWS, S0, S1, S2)).

%% AWS above S2 gets clamped to S2 before use.
upwash_aws_capped_at_s2_test() ->
    AWA = 0.4, S0 = 0.1, S1 = 0.05, S2 = 15.0,
    R1 = wind:correct_awa_from_upwash(AWA, 15.0, S0, S1, S2),
    R2 = wind:correct_awa_from_upwash(AWA, 99.0, S0, S1, S2),
    ?assertClose(R1, R2).

%%% ---------------------------------------------------------------
%%% correct_awa_from_heel/2
%%% ---------------------------------------------------------------

heel_zero_identity_test() ->
    ?assertClose(0.3, wind:correct_awa_from_heel(0.3, 0.0)).

%% cos(Heel) is even, so +Heel and -Heel must yield the same corrected AWA.
heel_sign_symmetry_test() ->
    AWA = 0.4, Heel = 0.2,
    ?assertClose(
        wind:correct_awa_from_heel(AWA, Heel),
        wind:correct_awa_from_heel(AWA, -Heel)
    ).

%% Known-value check derived from the formula.
heel_value_test() ->
    AWA = 0.4, Heel = 0.3,
    Expected = math:atan(math:tan(AWA) / math:cos(Heel)),
    ?assertClose(Expected, wind:correct_awa_from_heel(AWA, Heel)).

%% AWA in (pi/2, pi] lands in the correct quadrant (atan alone would fold it).
heel_above_half_pi_test() ->
    AWA = ?PI * 0.6, Heel = 0.2,
    R = wind:correct_awa_from_heel(AWA, Heel),
    ?assert(R > ?PI / 2).

heel_below_neg_half_pi_test() ->
    AWA = -?PI * 0.6, Heel = 0.2,
    R = wind:correct_awa_from_heel(AWA, Heel),
    ?assert(R < -?PI / 2).

%%% ---------------------------------------------------------------
%%% correct_heading_from_variation/2
%%% ---------------------------------------------------------------

variation_test() ->
    ?assertClose(1.2, wind:correct_heading_from_variation(1.0, 0.2)).

variation_negative_test() ->
    ?assertClose(0.8, wind:correct_heading_from_variation(1.0, -0.2)).

%%% ---------------------------------------------------------------
%%% leeway/3
%%% ---------------------------------------------------------------

%% Below 1 knot, leeway is forced to 0 to avoid blow-up from BSP^2 in denom.
leeway_low_speed_is_zero_test() ->
    ?assertEqual(0, wind:leeway(10.0, 0.5, 0.1)),
    ?assertEqual(0, wind:leeway(10.0, 0.0, 0.1)).

%% At BSP >= 1, formula = K*Heel / BSP^2.
leeway_value_test() ->
    ?assertClose(10.0 * 0.2 / (4.0 * 4.0), wind:leeway(10.0, 4.0, 0.2)).

leeway_sign_follows_heel_test() ->
    Pos = wind:leeway(10.0, 4.0, 0.2),
    Neg = wind:leeway(10.0, 4.0, -0.2),
    ?assertClose(Pos, -Neg).

%%% ---------------------------------------------------------------
%%% speed_through_water/2
%%% ---------------------------------------------------------------

stw_zero_leeway_test() ->
    ?assertClose(5.0, wind:speed_through_water(0.0, 5.0)).

stw_with_leeway_test() ->
    ?assertClose(5.0 / math:cos(0.1), wind:speed_through_water(0.1, 5.0)).

%% STW >= BSP always (since |cos(Leeway)| <= 1 in the valid range).
stw_geq_bsp_test() ->
    ?assert(wind:speed_through_water(0.1, 5.0) >= 5.0),
    ?assert(wind:speed_through_water(-0.1, 5.0) >= 5.0).

%%% ---------------------------------------------------------------
%%% true_wind/5
%%% ---------------------------------------------------------------

%% Pure headwind: TWS = AWS - BSP, TWA = 0, TWD = HDG.
true_wind_headwind_test() ->
    {TWA, TWS, TWD} = wind:true_wind(0.0, 10.0, 5.0, 0.0, 0.0),
    ?assertClose(TWS, 5.0),
    ?assertClose(TWA, 0.0),
    ?assertClose(TWD, 0.0).

%% Tailwind (AWA = pi): TWS = AWS + BSP, TWA = pi.
true_wind_tailwind_test() ->
    {TWA, TWS, _TWD} = wind:true_wind(?PI, 5.0, 3.0, 0.0, 0.0),
    ?assertClose(TWS, 8.0),
    ?assertClose(TWA, ?PI, 1.0e-5).

%% Beam wind from starboard: AWA=+pi/2, AWS=10, BSP=5.
%% Forward component of boat motion adds to TW_Y.
true_wind_beam_starboard_test() ->
    {TWA, TWS, _TWD} = wind:true_wind(?PI / 2, 10.0, 5.0, 0.0, 0.0),
    ?assertClose(TWS, math:sqrt(125.0), 1.0e-5),
    %% Expected TWA = 3pi/2 - atan2(5, -10).
    Expected = 3 * ?PI / 2 - math:atan2(5.0, -10.0),
    ?assertClose(TWA, Expected, 1.0e-5).

%% Port/starboard symmetry: flipping AWA's sign flips TWA's sign.
true_wind_sign_symmetry_test() ->
    {TWA_s, TWS_s, _} = wind:true_wind(?PI / 2, 10.0, 5.0, 0.0, 0.0),
    {TWA_p, TWS_p, _} = wind:true_wind(-?PI / 2, 10.0, 5.0, 0.0, 0.0),
    ?assertClose(TWS_s, TWS_p, 1.0e-5),
    ?assertClose(TWA_s, -TWA_p, 1.0e-5).

%% HDG adds directly to TWD in the simple headwind case.
true_wind_hdg_rotates_twd_test() ->
    {_, _, TWD} = wind:true_wind(0.0, 10.0, 5.0, 0.0, ?PI / 4),
    ?assertClose(TWD, ?PI / 4).

%% Headwind with positive leeway: boat slides to starboard, so the true
%% wind appears to come slightly from port. Closed form derivable from
%% the geometry: lateral boat-velocity component = BSP*tan(Leeway).
true_wind_headwind_with_leeway_test() ->
    AWS = 10.0, BSP = 5.0, Leeway = 0.1,
    {TWA, TWS, _} = wind:true_wind(0.0, AWS, BSP, Leeway, 0.0),
    Lat = BSP * math:tan(Leeway),
    ExpectedTWS = math:sqrt(Lat * Lat + (AWS - BSP) * (AWS - BSP)),
    ExpectedTWA = math:atan2(-Lat, AWS - BSP),
    ?assertClose(TWS, ExpectedTWS),
    ?assertClose(TWA, ExpectedTWA),
    %% Sanity: TWA should be small and negative (~ -Leeway for small L).
    ?assert(TWA < 0).

%% Cross-check true_wind/5 against first-principles vector physics in
%% the boat frame (x = starboard, y = bow):
%%   AW_vec  (direction wind is going) = (-AWS*sin(AWA), -AWS*cos(AWA))
%%   V_boat                            = ( STW*sin(L),    BSP)
%%   TW_vec = AW_vec + V_boat
%%   TWA = atan2(-TW_x, -TW_y)   (angle wind comes FROM, stbd positive)
%% Starboard-tack case (AWA > 0): exercises the AWA >= 0 wrap branch.
true_wind_leeway_vector_physics_stbd_test() ->
    AWA = 0.6, AWS = 12.0, BSP = 4.0, Leeway = 0.07, HDG = 0.0,
    {TWA, TWS, _} = wind:true_wind(AWA, AWS, BSP, Leeway, HDG),
    STW = BSP / math:cos(Leeway),
    TWx = -AWS * math:sin(AWA) + STW * math:sin(Leeway),
    TWy = -AWS * math:cos(AWA) + BSP,
    ?assertClose(TWS, math:sqrt(TWx * TWx + TWy * TWy)),
    ?assertClose(TWA, math:atan2(-TWx, -TWy)).

%% Port-tack case (AWA < 0): exercises the other wrap branch.
true_wind_leeway_vector_physics_port_test() ->
    AWA = -0.9, AWS = 8.0, BSP = 3.0, Leeway = -0.05, HDG = 0.0,
    {TWA, TWS, _} = wind:true_wind(AWA, AWS, BSP, Leeway, HDG),
    STW = BSP / math:cos(Leeway),
    TWx = -AWS * math:sin(AWA) + STW * math:sin(Leeway),
    TWy = -AWS * math:cos(AWA) + BSP,
    ?assertClose(TWS, math:sqrt(TWx * TWx + TWy * TWy)),
    ?assertClose(TWA, math:atan2(-TWx, -TWy)).

%% Mirroring AWA and Leeway across the centerline mirrors TWA and
%% leaves TWS unchanged.
true_wind_leeway_sign_symmetry_test() ->
    AWA = 0.4, AWS = 10.0, BSP = 5.0, Leeway = 0.08,
    {TWA_a, TWS_a, _} = wind:true_wind(AWA, AWS, BSP, Leeway, 0.0),
    {TWA_b, TWS_b, _} = wind:true_wind(-AWA, AWS, BSP, -Leeway, 0.0),
    ?assertClose(TWS_a, TWS_b),
    ?assertClose(TWA_a, -TWA_b).

%% Leeway -> 0 recovers the no-leeway solution.
true_wind_small_leeway_continuity_test() ->
    AWA = 0.4, AWS = 10.0, BSP = 5.0,
    {TWA0, TWS0, _} = wind:true_wind(AWA, AWS, BSP, 0.0, 0.0),
    {TWA1, TWS1, _} = wind:true_wind(AWA, AWS, BSP, 1.0e-8, 0.0),
    ?assertClose(TWA0, TWA1, 1.0e-6),
    ?assertClose(TWS0, TWS1, 1.0e-6).

%% HDG just rotates TWD in the ground frame; TWA and TWS don't depend on it.
true_wind_leeway_hdg_invariance_test() ->
    AWA = 0.4, AWS = 10.0, BSP = 5.0, Leeway = 0.08,
    {TWA_a, TWS_a, TWD_a} = wind:true_wind(AWA, AWS, BSP, Leeway, 0.0),
    {TWA_b, TWS_b, TWD_b} = wind:true_wind(AWA, AWS, BSP, Leeway, ?PI / 3),
    ?assertClose(TWA_a, TWA_b),
    ?assertClose(TWS_a, TWS_b),
    %% TWD_b should be TWD_a + pi/3, modulo 2*pi.
    Diff = math:fmod(TWD_b - TWD_a - ?PI / 3 + 4 * ?PI, 2 * ?PI),
    ?assert(Diff < 1.0e-6 orelse abs(Diff - 2 * ?PI) < 1.0e-6).

%%% ---------------------------------------------------------------
%%% vmg/3
%%% ---------------------------------------------------------------

%% Beam reach (TWA = pi/2), no leeway: VMG to wind = 0.
vmg_beam_reach_test() ->
    ?assertClose(0.0, wind:vmg(6.0, ?PI / 2, 0.0), 1.0e-6).

%% Head-to-wind (TWA = 0): VMG = STW.
vmg_into_wind_test() ->
    ?assertClose(6.0, wind:vmg(6.0, 0.0, 0.0)).

%% TWA0 > pi gets mapped into the negative half; TWA0 = 3pi/2 -> TWA = -pi/2,
%% so VMG = STW * cos(pi/2) = 0.
vmg_wraps_twa_above_pi_test() ->
    ?assertClose(0.0, wind:vmg(6.0, 3 * ?PI / 2, 0.0), 1.0e-6).

%% Leeway shifts the cosine argument; check against the raw formula.
vmg_with_leeway_test() ->
    STW = 6.0, TWA = 0.7, Leeway = 0.05,
    ?assertClose(STW * math:cos(-TWA + Leeway), wind:vmg(STW, TWA, Leeway)).

%%% ---------------------------------------------------------------
%%% rad_abs_to_rel/1 and rad_rel_to_abs/1
%%% ---------------------------------------------------------------

abs_to_rel_small_angle_test() ->
    ?assertClose(0.5, wind:rad_abs_to_rel(0.5)).

abs_to_rel_above_pi_test() ->
    ?assertClose(-?PI / 2, wind:rad_abs_to_rel(3 * ?PI / 2)).

abs_to_rel_at_pi_test() ->
    %% pi itself is not > pi, so it stays at pi.
    ?assertClose(?PI, wind:rad_abs_to_rel(?PI)).

rel_to_abs_positive_test() ->
    ?assertClose(0.5, wind:rad_rel_to_abs(0.5)).

rel_to_abs_negative_test() ->
    ?assertClose(3 * ?PI / 2, wind:rad_rel_to_abs(-?PI / 2)).

rel_to_abs_zero_test() ->
    ?assertClose(0.0, wind:rad_rel_to_abs(0.0)).

%% Round-trip: abs -> rel -> abs is the identity.
abs_rel_roundtrip_test() ->
    [
        begin
            R = wind:rad_rel_to_abs(wind:rad_abs_to_rel(A)),
            ?assertClose(R, A, 1.0e-5)
        end
     || A <- [0.0, 0.5, ?PI / 2, ?PI, 3 * ?PI / 2, 2 * ?PI - 0.01]
    ].

%%% ---------------------------------------------------------------
%%% damped_true_wind/7
%%% ---------------------------------------------------------------
%%%
%%% All scenarios use Leeway = 0, so STW = BSP and the filter's
%%% instantaneous true-wind computation is well-defined independent
%%% of how the BSP/STW argument is passed to true_wind/5.

init_default_test() ->
    %% Smoke test: init/0 and init/1 both produce a usable state.
    S0 = wind:damped_true_wind_init(),
    S1 = wind:damped_true_wind_init(5.0),
    {_, _, _, _} = wind:damped_true_wind(0, 0.0, 10.0, 5.0, 0.0, 0.0, S0),
    {_, _, _, _} = wind:damped_true_wind(0, 0.0, 10.0, 5.0, 0.0, 0.0, S1).

%% First sample must seed the filter with the instantaneous TW vector
%% (no smoothing applied yet), so the output equals what true_wind/5
%% would return.
first_sample_is_instantaneous_test() ->
    S0 = wind:damped_true_wind_init(10.0),
    {TWA, TWS, TWD, _} =
        wind:damped_true_wind(0, 0.0, 10.0, 5.0, 0.0, 0.0, S0),
    %% Headwind: AWS=10, BSP=5 => TWS=5, TWA=TWD=0.
    ?assert(abs(TWS - 5.0) < 1.0e-6),
    ?assert(abs(TWA) < 1.0e-6),
    ?assert(abs(TWD) < 1.0e-6).

%% With constant inputs the filter converges to the instantaneous value.
converges_to_steady_state_test() ->
    S0 = wind:damped_true_wind_init(2.0),
    Feed =
        fun(I, Acc) ->
            {_, _, _, Sx} =
                wind:damped_true_wind(I * 100, 0.0, 10.0, 5.0, 0.0, 0.0, Acc),
            Sx
        end,
    S = lists:foldl(Feed, S0, lists:seq(0, 200)),
    {TWA, TWS, TWD, _} =
        wind:damped_true_wind(20100, 0.0, 10.0, 5.0, 0.0, 0.0, S),
    ?assert(abs(TWS - 5.0) < 1.0e-3),
    ?assert(abs(TWA) < 1.0e-3),
    ?assert(abs(TWD) < 1.0e-3).

%% Core promise of the ground-frame filter: TWA snaps to the new heading
%% immediately, while TWD stays earth-stable.
%%
%% Setup: settle on headwind from north at HDG=0 (TWD=0, TWS=5).
%% Then the boat yaws east (HDG=pi/2) with the real (earth) wind
%% unchanged; the sensor reading that corresponds to this is
%% AWA = -pi/4, AWS = sqrt(50) (verified analytically).
heading_snap_test() ->
    S0 = wind:damped_true_wind_init(5.0),
    Feed =
        fun(I, Acc) ->
            {_, _, _, Sx} =
                wind:damped_true_wind(I * 100, 0.0, 10.0, 5.0, 0.0, 0.0, Acc),
            Sx
        end,
    S1 = lists:foldl(Feed, S0, lists:seq(0, 200)),
    {TWA, TWS, TWD, _} =
        wind:damped_true_wind(
            20100, -?PI / 4, math:sqrt(50.0), 5.0, ?PI / 2, 0.0, S1
        ),
    ?assert(abs(TWS - 5.0) < 1.0e-2),
    ?assert(abs(TWD) < 1.0e-2),
    ?assert(abs(TWA + ?PI / 2) < 1.0e-2).

%% First-order EMA with time-constant Tau: after Dt = Tau, the output
%% should move (1 - e^-1) ~= 63.2% of the way from old to new value.
ema_time_constant_test() ->
    Tau = 10.0,
    S0 = wind:damped_true_wind_init(Tau),
    %% Seed at TWS=5 (AWS=10, BSP=5).
    {_, _, _, S1} =
        wind:damped_true_wind(0, 0.0, 10.0, 5.0, 0.0, 0.0, S0),
    %% Step: AWS=15 => instantaneous TWS=10. Sample at t=Tau seconds.
    {_, TWS, _, _} =
        wind:damped_true_wind(
            round(Tau * 1000), 0.0, 15.0, 5.0, 0.0, 0.0, S1
        ),
    Expected = 5.0 + (1.0 - math:exp(-1.0)) * (10.0 - 5.0),
    ?assert(abs(TWS - Expected) < 1.0e-3).

%% A second sample at the same timestamp as the previous one yields
%% Dt = 0, A = 0, so the filter state does not advance.
zero_dt_does_not_advance_test() ->
    S0 = wind:damped_true_wind_init(10.0),
    {_, _, _, S1} =
        wind:damped_true_wind(1000, 0.0, 10.0, 5.0, 0.0, 0.0, S0),
    %% Different reading, same timestamp => output unchanged.
    {_, TWS, TWD, _} =
        wind:damped_true_wind(1000, 0.0, 20.0, 5.0, 0.0, 0.0, S1),
    ?assert(abs(TWS - 5.0) < 1.0e-6),
    ?assert(abs(TWD) < 1.0e-6).

%% A negative Dt (time going backwards) is clamped to 0 by dtw_lpf, so
%% state is not advanced (and A does not blow up).
backwards_time_is_clamped_test() ->
    S0 = wind:damped_true_wind_init(10.0),
    {_, _, _, S1} =
        wind:damped_true_wind(5000, 0.0, 10.0, 5.0, 0.0, 0.0, S0),
    {_, TWS, TWD, _} =
        wind:damped_true_wind(1000, 0.0, 20.0, 5.0, 0.0, 0.0, S1),
    ?assert(abs(TWS - 5.0) < 1.0e-6),
    ?assert(abs(TWD) < 1.0e-6).
