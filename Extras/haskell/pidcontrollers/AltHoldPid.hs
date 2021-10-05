{--
  PID controller for altitude hold

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

{-# LANGUAGE RebindableSyntax #-}

module AltHoldPid

where

import Language.Copilot

import PidController
import State
import Demands
import Utils

windupMax = 0.4 :: SFloat
pilotVelZMax = 2.5 :: SFloat
stickDeadband = 0.2 :: SFloat

altHoldController :: Stream Float -> Stream Float -> PidFun

altHoldController kp ki (state, rdemands, pdemands) = (state, rdemands, pdemands')

  where

    pdemands' = Demands throttleDemand (roll pdemands) (pitch pdemands) (yaw pdemands)

    throttleDemand =  kp * err + ki * errI

    -- Compute error as altTarget velocity minus actual velocity, after
    -- negating actual to accommodate NED
    err = targetVelocity + (dz state)

    -- Accumualte error integral
    errI = constrain_abs (errI' + err) windupMax

    targetVelocity = if inband then altitudeTarget - altitude else pilotVelZMax * (throttle rdemands)

    -- Reset controller when moving into deadband
    altitudeTarget = if inband && not (in_band throttleDemand' stickDeadband)
                     then altitude
                     else altitudeTarget'
    -- NED => ENU
    altitude = -(z state)

    -- inband = in_band throttleDemand stickDeadband
    inband = in_band (throttle rdemands) stickDeadband

    -- Controller state
    errI' = [0] ++ errI
    altitudeTarget' = [0] ++ altitudeTarget
    throttleDemand' = [0] ++ throttleDemand
