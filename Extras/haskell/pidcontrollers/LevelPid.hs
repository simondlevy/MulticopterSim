{--
  PID controller for roll/pitch angle

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

module LevelPid(levelController)

where

import Language.Copilot

import State(phi, theta)
import PidController
import Demands
import Utils

angleMax = 45 :: SFloat

levelController :: SFloat -> PidFun

levelController kp (state, rdemands, pdemands) = (state, rdemands, pdemands')

    where

      pdemands' = Demands (throttle pdemands) rollDemand pitchDemand (yaw pdemands)

      rollDemand = ((kp * demandScale * (roll pdemands)) - (phi state))

      -- Pitch demand is nose-down positive, so we negate pitch-forward
      -- vehicle state (nose-down negative) to reconcile them
      pitchDemand = ((kp * demandScale * (pitch pdemands)) + (theta state))

      -- angle for error computation, we multiply by the following amount:
      demandScale = 2 * (deg2rad angleMax)
