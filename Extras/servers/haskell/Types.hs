{--
  Types for Multicopter

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

module Types where

data Time = Time Double deriving Show
timeval :: Time -> Double
timeval (Time v) = v

------------------------------------

data State = State [Double] deriving Show

statevals :: State -> [Double]
statevals (State vs) = vs

state_x :: State -> Double
state_x (State vs) = vs !! 0

state_dx :: State -> Double
state_dx (State vs) = vs !! 1

state_y :: State -> Double
state_y (State vs) = vs !! 2

state_dy :: State -> Double
state_dy (State vs) = vs !! 3

state_z :: State -> Double
state_z (State vs) = vs !! 4

state_dz :: State -> Double
state_dz (State vs) = vs !! 5

state_phi :: State -> Double
state_phi (State vs) = vs !! 6

state_dphi :: State -> Double
state_dphi (State vs) = vs !! 7

state_theta :: State -> Double
state_theta (State vs) = vs !! 8

state_dtheta :: State -> Double
state_dtheta (State vs) = vs !! 9

state_psi :: State -> Double
state_psi (State vs) = vs !! 10

state_dpsi :: State -> Double
state_dpsi (State vs) = vs !! 11

------------------------------------

data Motors = Motors [Double] deriving Show
motorvals :: Motors -> [Double]
motorvals (Motors vs) = vs

------------------------------------

type ControlFunc = Time -> State -> Motors
