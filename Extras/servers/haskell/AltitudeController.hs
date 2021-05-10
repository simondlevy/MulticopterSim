{--
  PID Controller

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

module AltitudeController where

data Constants = Constants { kp_z  :: Double
                           , kp_dz :: Double
                           , ki_dz :: Double 
                           , windupMax :: Double } deriving (Show)

data Controller = Controller { constants :: Constants
                             , lastError :: Double
                             , errorIntegral :: Double } deriving (Show)
