{--
  Types for Multicopter

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

module Types where

data State = State [Double] deriving Show

data Motors = Motors [Double] deriving Show
motorvals :: Motors -> [Double]
motorvals (Motors v) = v

type ControlFunc = [Double] -> [Double]
