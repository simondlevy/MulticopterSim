{--

  Copyright(C) 2021 Simon D.Levy

  This file is part of SimFlightControl.

  SimFlightControl is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  SimFlightControl is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
  more details.

  You should have received a copy of the GNU General Public License along with
  SimFlightControl. If not, see <https://www.gnu.org/licenses/>.

--}

import Types
import Multicopter
import AltitudeController

main :: IO ()
main = let constants = AltitudeControllerConstants 10 1 0 10
       in runMulticopter (makeAltitudeController constants) quadXAPMixer
