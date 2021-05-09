{--
  PID Controller

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

module PidController (PidController) where

data PidController = PidController Double Double Double Double deriving Show

pid_kp :: PidController -> Double
pid_kp (PidController kp _ki _kd _windupMax) = kp

pid_ki :: PidController -> Double
pid_ki (PidController _kp ki _kd _windupMax) = ki

pid_kd :: PidController -> Double
pid_kd (PidController _kp _ki kd _windupMax) = kd

pid_windup_max :: PidController -> Double
pid_windup_max (PidController _kp _ki _kd windupMax) = windupMax

makePidController :: Double -> Double -> Double -> PidController
makePidController kp ki kd = PidController kp ki kd 0

