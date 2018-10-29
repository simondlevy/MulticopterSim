/*
   simposhold.hpp : temporary PID-based position hold for simulator

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

#include "debug.hpp"
#include "datatypes.hpp"
#include "receiver.hpp"
#include "pidcontroller.hpp"

namespace hf {


    class PositionHold : public PID_Controller {

        friend class Hackflight;

        public:

        PositionHold(float cyclicP) 
        {
            _cyclicP = cyclicP;
        }

        protected:

        virtual bool modifyDemands(state_t & state, demands_t & demands, float currentTime) 
        {
            (void)currentTime;

            // Pitch/roll
            demands.pitch = adjustCyclic(demands.pitch, state.velocityForward);
            demands.roll  = adjustCyclic(demands.roll,  state.velocityRightward);

            return true;
        }

        virtual bool shouldFlashLed(void) override 
        {
            return true;
        }

        bool inBand(float demand)
        {
            return fabs(demand) < Receiver::STICK_DEADBAND; 
        }

        float adjustCyclic(float demand, float velocity)
        {
            // Inside throttle deadband, adjust pitch/roll demand by PD controller; outside deadband, leave it as-is
            return inBand(demand) ? demand - _cyclicP*velocity: demand; 
        }

        // set by constructor
        float _cyclicP;

    };  // class PositionHold

} // namespace hf
