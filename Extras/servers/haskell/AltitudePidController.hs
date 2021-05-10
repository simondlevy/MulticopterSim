{--
  PID Controller

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

module AltitudePidController (AltitudePidController) where

data AltitudePidController = AltitudePidController Double Double Double Double Double Double deriving Show

pid_kp :: AltitudePidController -> Double
pid_kp (AltitudePidController kp _ki _kd _windupMax _lastError _errorIntegral) = kp

pid_ki :: AltitudePidController -> Double
pid_ki (AltitudePidController _kp ki _kd _windupMax _lastError _errorIntegral) = ki

pid_kd :: AltitudePidController -> Double
pid_kd (AltitudePidController _kp _ki kd _windupMax _lastError _errorIntegral) = kd

pid_windup_max :: AltitudePidController -> Double
pid_windup_max (AltitudePidController _kp _ki _kd windupMax _lastError _errorIntegral) = windupMax

makeAltitudePidController :: Double -> Double -> Double -> AltitudePidController
makeAltitudePidController kp ki kd = AltitudePidController kp ki kd 0 0 0
