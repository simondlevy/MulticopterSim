/*
 * Abstract class for multicopter frames
 *
 * For reference citation see MultirotorDynamics.hpp
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include <stdint.h>

class MulticopterFrame {

    private:

        uint8_t   _motorCount;

        double ** _motorLocations; 

    protected:

        MulticopterFrame(const double ** motorLocations, uint8_t motorCount) 
        {
            _motorCount = motorCount;

            _motorLocations = new double * [motorCount];

            for (uint8_t i=0; i<motorCount; ++i) {
                _motorLocations[i] = new double[3];
                memcpy(_motorLocations[i], motorLocations[i], 3*sizeof(double));
            }
        }

        virtual ~MulticopterFrame(void)
        {
            for (uint8_t i=0; i<_motorCount; ++i) {
                delete _motorLocations[i];
            }
            delete _motorLocations;
         }

    public:

        // roll right
        virtual double u2(double * o) = 0;

        // pitch forward
        virtual double u3(double * o) = 0;

        // yaw cw
        virtual double u4(double * o) = 0;

}; // class MulticopterFrame

