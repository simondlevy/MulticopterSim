{--
  PID Controller

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

module AltitudePidController (make) where

import Types

data PidConstants = PidConstants { kp :: Double
                                 , ki :: Double
                                 , kd :: Double } deriving (Show)

data 

data AltitudePidController = AltitudePidController Double Double Double Double Double Double deriving Show

pid_kp :: AltitudePidController -> Double
pid_kp (AltitudePidController kp _ki _kd _windupMax _lastError _errorIntegral) = kp

pid_ki :: AltitudePidController -> Double
pid_ki (AltitudePidController _kp ki _kd _windupMax _lastError _errorIntegral) = ki

pid_kd :: AltitudePidController -> Double
pid_kd (AltitudePidController _kp _ki kd _windupMax _lastError _errorIntegral) = kd

pid_windup_max :: AltitudePidController -> Double
pid_windup_max (AltitudePidController _kp _ki _kd windupMax _lastError _errorIntegral) = windupMax

pid_last_error :: AltitudePidController -> Double
pid_last_error (AltitudePidController _kp _ki _kd _windupMax lastError _errorIntegral) = lastError

pid_integral_error :: AltitudePidController -> Double
pid_integral_error (AltitudePidController _kp _ki _kd _windupMax _lastError errorIntegral) = errorIntegral

make :: Double -> Double -> Double -> AltitudePidController
make kp ki kd = AltitudePidController kp ki kd 1 0 0

apply :: AltitudePidController -> State -> (Motors, AltitudePidController)
apply altPid state = ((Motors 0  0  0  0), altPid)
