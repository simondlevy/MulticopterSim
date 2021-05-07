{--
  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

import Types
import Multicopter

controlFunc :: ControlFunc
controlFunc _ = Motors [1,1,1,1]

main :: IO ()
main = runMulticopter controlFunc
