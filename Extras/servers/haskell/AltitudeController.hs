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

    \_time -> \vehicleState -> \_demands -> \_controllerState ->

    let  
         -- Get constants 
         target = altitude_target constants
         kp_z = altitude_Kp_z constants
         kp_dz = altitude_Kp_dz constants
         ki_dz = altitude_Ki_dz constants
         windupMax = altitude_windupMax constants

         -- Get vehicle state
         z = state_z vehicleState
         dzdt = state_dz vehicleState

         -- Compute dzdt setpoint and error
         dzdt_target = (target - z) * kp_z
         dzdt_error = dzdt_target - dzdt

        -- Update error integral and error derivative
        -- integralError += dzdt_error * dt
        -- integralError = constrainAbs(integralError + dzdt_error * dt, windupMax)

     u = 1

    in ((Demands u 0 0 0), (PidControllerState 0))
