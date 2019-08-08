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
        
        class Location {

            private:

                double _x, _y, _z;

            public:

                Location(double x, double y, double z) : _x(x), _y(y), _z(z) { }

                Location(const Location & loc): _x(loc._x), _y(loc._y), _z(loc._z) { }
        };

        static const uint16_t RES_COLS = 346;
        static const uint16_t RES_ROWS = 260;

        // Simplified AER event structure, based on https://arxiv.org/pdf/1510.01972.pdf
        typedef struct {

            uint32_t t; // time (microseconds)
            uint16_t x; // X coordinate
            uint16_t y; // X coordinate
            int8_t   p; // polarity +/-

        } event_t;

    private:

        // Field of view, assuming a lens with 4.5mm focal length. See:
        // https://inivation.com/support/hardware/davis346/#computations-of-field-of-view
        static constexpr double FOV_H = 70.8;
        static constexpr double FOV_V = 56.2;

        // Assume a spherical cow
        double _targetSize = 0;

        queue<event_t> _eventq;

        bool inView(const Location & vehicleLocation, const Location & targetLocation)
        {
            return true;
        }

    public:

        Davis346(double targetSize)
        {
            _targetSize = targetSize;
        }

        ~Davis346(void)
        {
        }

        void update(const Location & vehicleLocation, const Location & targetLocation)
        {
            if (!inView(vehicleLocation, targetLocation)) return;

            //debug("vehicle: %s    target: %s", vehicleLocation.toString(), targetLocation.toString());
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
