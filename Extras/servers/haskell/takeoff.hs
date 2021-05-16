{--
  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

import Types
import Multicopter
import AltitudeController

main :: IO ()
main = let constants = AltitudeController.AltitudeControllerConstants 10 1 1 0 10
       in runMulticopter (makeAltitudeController constants) quadXAPMixer
