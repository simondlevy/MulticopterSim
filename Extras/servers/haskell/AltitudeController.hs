{--
  Altitude-hold PID Controller

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

module AltitudeController(AltitudeControllerConstants(..), makeAltitudeController) where


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

    \time -> \vehicleState -> \_demands -> \controllerState ->

    let  
         -- Get constants 
         ztarget = altitude_target constants
         kp_z = altitude_Kp_z constants
         kp_dz = altitude_Kp_dz constants
         ki_dz = altitude_Ki_dz constants
         windupMax = altitude_windupMax constants

         -- Get vehicle state, negating for NED
         z = -(state_z vehicleState)
         dzdt = -(state_dz vehicleState)

         -- Compute dzdt setpoint and error
         dzdt_target = (ztarget - z) * kp_z
         dzdt_error = dzdt_target - dzdt

         -- Update error integral
         dt = time - (previousTime controllerState)
         newErrorIntegral = constrainAbs ((errorIntegral controllerState) + dzdt_error * dt) windupMax

         -- Compute throttle demand, constrained to [0,1]
         u = min (kp_dz * dzdt_error + ki_dz * newErrorIntegral) 1

    in ((Demands u 0 0 0), (PidControllerState time newErrorIntegral))

constrainAbs :: Double -> Double -> Double
constrainAbs x lim = if x < -lim then -lim else (if x > lim then lim else x)

