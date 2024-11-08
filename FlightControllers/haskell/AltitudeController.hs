{--

  Altitude-hold PID Controller

  Copyright(C) 2021 Simon D.Levy

  This file is part of SimFlightControl.

  SimFlightControl is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  SimFlightControl is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
  more details.

  You should have received a copy of the GNU General Public License along with
  SimFlightControl. If not, see <https://www.gnu.org/licenses/>.

--}

module AltitudeController(AltitudeControllerConstants(..), makeAltitudeController) where


import Types

data AltitudeControllerConstants =
    AltitudeControllerConstants {
      altitude_target ::Double
    , altitude_Kp ::Double
    , altitude_Ki ::Double
    , altitude_windupMax ::Double
    } deriving (Show)
             
type AltitudeController = AltitudeControllerConstants -> PidController

makeAltitudeController :: AltitudeController

makeAltitudeController constants =

    \time -> \vehicleState -> \_demands -> \controllerState ->

    let  
         -- Get constants 
         ztarget = altitude_target constants
         kp = altitude_Kp constants
         ki = altitude_Ki constants
         windupMax = altitude_windupMax constants

         -- Get vehicle state
         z = state_z vehicleState
         dzdt = state_dz vehicleState

         -- Compute dzdt setpoint and error
         dzdt_error = (ztarget - z) - dzdt

         -- Update error integral
         dt = time - (previousTime controllerState)
         newErrorIntegral = constrainAbs ((errorIntegral controllerState) + dzdt_error * dt) windupMax

         -- Compute throttle demand, constrained to [0,1]
         u = min (kp * dzdt_error + ki * newErrorIntegral) 1

    in ((Demands u 0 0 0), (PidControllerState time newErrorIntegral))

constrainAbs :: Double -> Double -> Double
constrainAbs x lim = if x < -lim then -lim else (if x > lim then lim else x)

