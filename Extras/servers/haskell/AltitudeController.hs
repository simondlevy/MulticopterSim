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
    , altitude_windupMax ::Double
    } deriving (Show)
             
type AltitudeController = AltitudeControllerConstants -> PidController

makeAltitudeController :: AltitudeController

makeAltitudeController constants =

    \_time -> \_vehicleState -> \_demands -> \_controllerState ->

    let  target = altitude_target constants
         kp_z = altitude_Kp_z constants
         kp_dz = altitude_Kp_dz constants
         ki_dz = altitude_Ki_dz constants
         windupMax = altitude_windupMax constants
         -- velTarget = (target - alt) * self.Kp_pos
         u = 1

    in ((Demands u 0 0 0), (PidControllerState 0))
