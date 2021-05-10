{--
  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

import Types
import Multicopter
import AltitudeController

takeoffControl :: ClosedLoopController
takeoffControl _time state = 
    let altitude = -(state_z state) -- NED => ENU
        target = 10
        err = target - altitude
        u = 0.5 - 0.00001 * err
    in Demands u 0 0 0

main :: IO ()
main = do
    runMulticopter takeoffControl quadXAPMixer
