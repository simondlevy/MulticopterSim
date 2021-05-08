{--
  PID Controller

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

module PidController (PidController) where


data PidController = PidController
  { kp :: Double
  , ki :: Double
  , kd :: Double }

