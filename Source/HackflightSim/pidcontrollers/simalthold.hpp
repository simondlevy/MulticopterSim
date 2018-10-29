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

#include "debug.hpp"
#include "datatypes.hpp"
#include "receiver.hpp"
#include "pidcontroller.hpp"

namespace hf {

    class SimAltitudeHold : public PID_Controller {

        friend class Hackflight;

        private:

        // Arbitrary constants
        const float WINDUP_MAX      = 0.40f;
        const float HOVER_THROTTLE  = 0.05f;

        // set by constructor
        float _altHoldP;
        float _altitudeD;
        float _minAltitude;

        // modified in-flight
        float _altitudeTarget;
        bool  _inBandPrev;
        float _lastError;
        float _deltaError;
        float _integralError;
        float _velocityTarget;
        float _previousTime;

        bool inBand(float demand)
        {
            return fabs(demand) < Receiver::STICK_DEADBAND; 
        }

        void resetErrors(void)
        {
            _lastError = 0;
            _deltaError = 0;
            _integralError = 0;
        }


        virtual float correctedThrottle(state_t & state, float dt)
        {
            return _altHoldP * (_altitudeTarget-state.altitude) - _altitudeD * state.variometer;
        }

        protected:

        virtual bool modifyDemands(state_t & state, demands_t & demands, float currentTime) 
        {
            // Don't do anything till we've reached sufficient altitude
            if (state.altitude < _minAltitude) return false;

            // Don't do anything until we have a positive dt
            float dt = currentTime - _previousTime;
            _previousTime = currentTime;
            if (dt == currentTime) return false;

            // Reset altitude target if moved into stick deadband
            bool inBandCurr = inBand(demands.throttle);
            if (inBandCurr && !_inBandPrev) {
                _altitudeTarget = state.altitude;
            }
            _inBandPrev = inBandCurr;


            // Throttle: inside stick deadband, adjust by P(PID);
            // outside deadband, respond to stick demand
            demands.throttle = inBandCurr ? correctedThrottle(state, dt) : demands.throttle;

            return inBandCurr;
        }

        virtual bool shouldFlashLed(void) override 
        {
            return true;
        }

        public:

        SimAltitudeHold(float altHoldP, float altitudeD, float minAltitude=0.1)
        {
            _altHoldP   = altHoldP;
            _altitudeD   = altitudeD;
            _minAltitude = minAltitude;

            resetErrors();
            _previousTime = 0;
            _inBandPrev = false;
        }

    };  // class SimAltitudeHold

} // namespace hf
