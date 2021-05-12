{--
  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

import Types
import Multicopter
import AltitudeController

main :: IO ()
main = runMulticopter altitudeController quadXAPMixer
