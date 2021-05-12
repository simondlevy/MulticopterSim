{--
  PID Controller

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

module AltitudeController where

import Types

data AltitudeControllerConstants =
    AltitudeControllerConstants {
      altitude_target ::Double
    , altitude_Kp_z ::Double
    , altitude_Kp_dz ::Double
    , altitude_Ki_dz ::Double
    } deriving (Show)
             

altitudeController :: PidController

altitudeController _time _vehicleState _demands _controllerState =

    ((Demands 1 0 0 0), (PidControllerState 0))
