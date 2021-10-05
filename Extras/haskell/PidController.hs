{--
  PID controllers for aerial vehicles

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}


module PidController

where

import Language.Copilot

import State
import Demands

-- PID controller takes vehicle state, original receiver demands, and demands
-- outputfrom previous controller; returns updated demands.  Original receiver
-- demands are needed by certain PID controllers using stick deadband (altitude
-- hold, position hold) but not by others (rate, level, yaw)

type PidFun = (State, Demands, Demands) -> (State, Demands, Demands)
