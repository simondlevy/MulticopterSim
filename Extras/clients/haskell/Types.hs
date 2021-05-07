{--
  Types for Multicopter

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

module Types where

data State = State [Double] deriving Show

data Motors = Motors [Double] deriving Show

type ControlFunc = [Double] -> [Double]
