{--
  Types for Multicopter

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

module Types where

data Time = Time { time :: Double} deriving (Show)

-------------------------------------------------------

-- See Bouabdallah et al. (2004)
data VehicleState = VehicleState { 
                     state_x :: Double
                   , state_dx :: Double 
                   , state_y :: Double
                   , state_dy :: Double 
                   , state_z :: Double
                   , state_dz :: Double 
                   , state_phi :: Double
                   , state_dphi :: Double 
                   , state_theta :: Double
                   , state_dtheta :: Double 
                   , state_psi :: Double
                   , state_dpsi :: Double 
                   } deriving (Show)

-------------------------------------------------------

data Demands = Demands { throttle :: Double
                       , roll :: Double  
                       , pitch :: Double  
                       , yaw :: Double  
                     } deriving (Show)

-------------------------------------------------------

-- XXX should support different numbers of motors
data Motors = Motors { m1 :: Double
                     , m2 :: Double  
                     , m3 :: Double  
                     , m4 :: Double  
                     } deriving (Show)

-------------------------------------------------------

type Mixer = Demands -> Motors

quadXAPMixer :: Mixer
quadXAPMixer demands = 
    let t = (throttle demands)
        r = (roll demands)
        p = (pitch demands)
        y = (yaw demands)
    in Motors (t - r - p - y)
              (t + r + p - y)
              (t + r - p + y)
              (t - r + p + y)
     
-------------------------------------------------------

type ClosedLoopControllerState = [Double]

type ClosedLoopController = Time -> VehicleState -> ClosedLoopControllerState -> Demands
