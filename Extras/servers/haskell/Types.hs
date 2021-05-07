{--
  Types for Multicopter

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

module Types where

data Time = Time Double deriving Show
timeval :: Time -> Double
timeval (Time v) = v

data State = State [Double] deriving Show
statevals :: State -> [Double]
statevals (State vs) = vs

data Motors = Motors [Double] deriving Show
motorvals :: Motors -> [Double]
motorvals (Motors vs) = vs

type ControlFunc = Time -> State -> Motors
