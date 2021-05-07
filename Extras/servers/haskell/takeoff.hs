{--
  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

import Types
import Multicopter

altitudeHoldControl :: ControlFunc
altitudeHoldControl time state = 
    Motors [1,1,1,1]

main :: IO ()
main = runMulticopter altitudeHoldControl
