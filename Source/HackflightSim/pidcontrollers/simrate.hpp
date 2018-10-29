/*
   simrate.hpp : rate PID controller for HackflightSim

   XXX should eventually use same code as Hackflight

   Copyright (c) 2018 Juan Gallostra and Simon D. Levy

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

#include "pidcontrollers/rate.hpp"

namespace hf {

    class SimRate : public Rate {

        private:
            
            virtual float computePid(float rateP, float PTerm, float ITerm, float DTerm, float gyro[3], uint8_t axis) override
            {
                //PTerm = (PTerm * _demandsToRate - gyro[axis]) * rateP;
				PTerm = PTerm - (gyro[axis] * rateP);

                return PTerm + ITerm + DTerm;
            }

        public:

			SimRate(float rollPitchP, float rollPitchI, float rollPitchD, float yawP, float yawI,
				float demandsToRate) : Rate(rollPitchP, rollPitchI, rollPitchD, yawP, yawI, demandsToRate)
			{

			}

    };  // class SimRate

} // namespace hf
