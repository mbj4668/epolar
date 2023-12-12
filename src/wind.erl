-module(wind).

%% http://sailboatinstruments.blogspot.com/2011/05/\
%%   true-wind-vmg-and-current-calculations.html

-export([correct_awa_from_alignment_offset/2,
         correct_awa_from_heel/2,
         correct_heading_from_variation/2,
         leeway/3,
         speed_through_water/2,
         true_wind/5,
         vmg/3]).
-export([rad_abs_to_rel/1, rad_rel_to_abs/1]).

-type knots() :: number(). % resolution 1
-type rad() :: number().   % resolution 1

%% relative to boat axis, port is negative
-type angle_rad_rel() :: angle(rad). % -?PI - ?PI
-type angle_rad_abs() :: angle(rad). % 0 - 2*?PI
-type angle(_UR) :: number(). % any unit, any resolution.
-type speed(_UR) :: number(). % any unit, any resolution.

-define(PI, 3.14159265).
-define(DEG_TO_RAD, (?PI/180)).
-define(RAD_TO_DEG, (180/?PI)).
-define(MS_TO_KNOTS, (3600/1852)).
-define(KNOTS_TO_MS, (1852/3600)).

-spec correct_awa_from_alignment_offset(AWA :: angle_rad_abs(),
                                        Offset :: angle(rad)) ->
          AWA :: angle_rad_abs().
correct_awa_from_alignment_offset(AWA, Offset) ->
    CAWA = AWA + Offset,
    if CAWA > ?PI ->
            CAWA - (2 * ?PI);
       CAWA < -?PI ->
            CAWA + (2 * ?PI);
       true ->
            CAWA
    end.

-spec correct_awa_from_heel(AWA :: angle_rad_rel(),
                            Heel :: angle_rad_rel()) ->
          AWA :: angle_rad_rel().
correct_awa_from_heel(AWA, Heel) ->
    TanAWA = math:tan(AWA),
    CosHeel = math:cos(Heel),
    AWAHeel0 = math:atan(TanAWA / CosHeel),
    AWAHeel =
        if AWA > ?PI/2 ->
                AWAHeel0 + ?PI;
           AWA < -?PI/2 ->
                AWAHeel0 - ?PI;
           true ->
                AWAHeel0
        end,
    AWAHeel.

-spec correct_heading_from_variation(HDG :: angle(T),
                                     Variation :: angle(T)) ->
          HGD :: angle(T).
correct_heading_from_variation(HDG, Variation) ->
    HDG + Variation.

-spec leeway(K :: number(),
             BoatSpeed :: knots(),
             Heel :: angle(T)) ->
          angle(T).
%% Heel and Leeway are positive when the mast leans to starboard (port
%% tack) and negative when the mast leans to port (starboard tack).
leeway(K, BoatSpeed, Heel) ->
    if BoatSpeed < 1 ->
            0;
       true ->
            Leeway = K * Heel / (BoatSpeed * BoatSpeed),
            Leeway
    end.

-spec speed_through_water(Leeway :: rad(), BSP :: speed(T)) ->
          speed(T).
speed_through_water(Leeway, BSP) ->
    STW = BSP / math:cos(Leeway),
    STW.

-spec true_wind(AWA :: angle_rad_rel(),
                AWS :: speed(T),
                BoatSpeed :: speed(T),
                Leeway :: rad(),
                Heading :: rad()) ->
          {TWA :: angle_rad_rel(), TWS :: speed(T), TWD :: rad()}.
%% Calculate true wind angle, speed and direction (TWA, TWS, TWD).
%% If Heading is not known, set it to 0.  TWD will not be correct.
%% AWA and TWA are relative to boat axis, negative on port.
true_wind(AWA, AWS, BoatSpeed, Leeway, HDG) ->
    CAWA = 3*?PI/2 - AWA,
    %% Calculate component of boat speed perpendicular to boat axis
    LateralSpeed = BoatSpeed * math:sin(Leeway),
    AWS_X = AWS * math:cos(CAWA),
    AWS_Y = AWS * math:sin(CAWA),
    TWS_X = AWS_X + LateralSpeed,
    TWS_Y = AWS_Y + BoatSpeed,
    TWS = math:sqrt(TWS_X * TWS_X + TWS_Y * TWS_Y),
    CTWA = math:atan2(TWS_Y, TWS_X),
    TWA0 = 3*?PI/2 - CTWA,
    TWA1 =
        if AWA >= 0 ->
                math:fmod(TWA0, 2*?PI);
           true ->
                TWA0 - 2*?PI
        end,
    TWA =
        if TWA1 > ?PI ->
                TWA1 - 2*?PI;
           TWA1 < -?PI ->
                TWA1 + 2*?PI;
           true ->
                TWA1
        end,
    %% Calculate True Wind Direction
    TWD0 = HDG + TWA,
    TWD =
        if TWD0 > 2*?PI ->
                TWD0 - 2*?PI;
           TWD0 < 0 ->
                TWD0 + 2*?PI;
           true ->
                TWD0
        end,
    {TWA, TWS, TWD}.


-spec vmg(STW :: speed(T), TWA0 :: rad(), Leeway :: rad()) ->
          speed(T).
vmg(STW, TWA0, Leeway) ->
    TWA =
        if TWA0 > ?PI ->
                TWA0 - 2*?PI;
           true ->
                TWA0
        end,
    VMG = STW * math:cos(-TWA + Leeway),
    VMG.

-spec rad_abs_to_rel(angle_rad_abs()) -> angle_rad_rel().
rad_abs_to_rel(Angle) ->
    if Angle > ?PI ->
            Angle - 2*?PI;
       true ->
            Angle
    end.

-spec rad_rel_to_abs(angle_rad_rel()) -> angle_rad_abs().
rad_rel_to_abs(Angle) ->
    if Angle < 0 ->
            2*?PI + Angle;
       true ->
            Angle
    end.
