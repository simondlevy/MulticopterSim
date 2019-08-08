/*
   Platform-indpendent simulator for iniVation DAVIS346 Dynamic Vision Sensor

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#pragma once

#include <queue>
using namespace std;

#include <stdint.h>

// Debugging support
#ifdef _WIN32
#include "../MainModule/Utils.hpp"
#else
#include <stdio.h>
#define debug printf
#endif

class Davis346 {

    public:
        
        static const uint16_t RES_COLS = 346;
        static const uint16_t RES_ROWS = 260;

        // Simplified AER event structure, based on https://arxiv.org/pdf/1510.01972.pdf
        typedef struct {

            uint32_t t; // time (microseconds)
            uint16_t x; // X coordinate
            uint16_t y; // X coordinate
            int8_t   p; // polarity +/-

        } event_t;

        typedef struct {

            double x;
            double y;
            double z;

        } location_t;

    private:

        // Field of view, assuming a lens with 4.5mm focal length. See:
        // https://inivation.com/support/hardware/davis346/#computations-of-field-of-view
        static constexpr double FOV_H = 70.8;
        static constexpr double FOV_V = 56.2;

        // Assume a spherical cow
        double _targetSize = 0;

        queue<event_t> _eventq;

    public:

        Davis346(double targetSize)
        {
            _targetSize = targetSize;
        }

        ~Davis346(void)
        {
        }

        void update(const location_t & vehicleLocation, const location_t & targetLocation)
        {
            debug("vehicle: %+3.3f %+3.3f %+3.3f    target: %+3.3f %+3.3f %+3.3f",
                    vehicleLocation.x, vehicleLocation.y, vehicleLocation.z,
                    targetLocation.x, targetLocation.y, targetLocation.z);
        }

        bool hasMoreEvents(void)
        {
            return !_eventq.empty();
        }

        event_t dequeueEvent(void)
        {
            event_t event = _eventq.front();
            _eventq.pop();
            return event;
        }
}; 
