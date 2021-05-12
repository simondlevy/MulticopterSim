{--
  PID Controller

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

module AltitudeController where

import Types

altitudeController :: PidController

altitudeController _time _vehicleState _demands _controllerState =

    ((Demands 1 0 0 0), (PidControllerState 0))
