/*
   simalthold.hpp : temporary PID-based altitude hold for simulator

   Copyright (c) 2018 Simon D. Levy

   This file is part of HackflightSim.

   HackflightSim is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   HackflightSim is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   You should have received a copy of the GNU General Public License
   along with Hackflight.  If not, see <http://www.gnu.org/licenses/>.
 */

#pragma once

#include "pidcontrollers/althold.hpp"

namespace hf {

    class SimAltitudeHold : public AltitudeHold {

        private:

        virtual float correctedThrottle(state_t & state, float dt) override
        {
			float velocityTarget = (_altitudeTarget - state.altitude) * _altHoldP;
            return velocityTarget - _altHoldVelP * state.variometer;
        }

        public:

        SimAltitudeHold(float altHoldP, float altHoldVelP) :
            AltitudeHold(altHoldP, altHoldVelP, 0, 0) 
		{
		}


    };  // class SimAltitudeHold

} // namespace hf
