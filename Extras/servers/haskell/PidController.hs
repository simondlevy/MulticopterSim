{--
  PID Controller

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

module PidController (PidController) where


data PidController = PidController Double Double Double deriving Show

pid_kp :: PidController -> Double
pid_kp (PidController kp _ki _kd) = kp

pid_ki :: PidController -> Double
pid_ki (PidController _kp ki _kd) = ki

pid_kd :: PidController -> Double
pid_kd (PidController _kp _ki kd) = kd

