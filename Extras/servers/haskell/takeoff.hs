{--
  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

import Types
import Multicopter

altitudeHoldControl :: ControlFunc
altitudeHoldControl time state = 
    if altitude < 10 then Motors [0.6,0.6,0.6,0.6] else Motors [0,0,0,0]
    where altitude = -(state_z state)

main :: IO ()
main = do
    runMulticopter altitudeHoldControl
