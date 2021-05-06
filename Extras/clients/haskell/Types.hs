{--
  Types for Multicopter

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

module Types (State(..), Motors(..), ControlFunc) where

data State = State Double Double Double Double Double Double Double Double Double Double Double Double deriving Show

data Motors = Motors Double Double Double Double deriving Show

type ControlFunc = [Double] -> [Double]
