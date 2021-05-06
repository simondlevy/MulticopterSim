{--
  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

import Types
import Multicopter

controlFunc :: ControlFunc
controlFunc _ = [1,1,1,1]

main :: IO ()
main = runMulticopter controlFunc
-- main = print (Motors 0.0 0.0 0.0 0.0)
