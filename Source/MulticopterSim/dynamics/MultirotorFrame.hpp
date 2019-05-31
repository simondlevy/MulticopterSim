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
#include <string.h>

class MultirotorFrame {

    private:

        uint8_t   _motorCount;

        double * _motorLocations; 

    protected:

        MultirotorFrame(const double * motorLocations, uint8_t motorCount) 
        {
            _motorCount = motorCount;

            _motorLocations = new double[3*motorCount];

            memcpy(_motorLocations, motorLocations, 3*motorCount*sizeof(double));
        }

        virtual ~MultirotorFrame(void)
        {
            delete _motorLocations;
        }

    public:

        virtual double b(void) = 0;
        virtual double d(void) = 0;
        virtual double m(void) = 0;
        virtual double Ix(void) = 0;
        virtual double Iy(void) = 0;
        virtual double Iz(void) = 0;
        virtual double Jr(void) = 0;
        virtual uint16_t maxrpm(void) = 0;

        uint8_t motorCount(void)
        {
            return _motorCount;
        }

        double l(void)
        {
            return fabs(_motorLocations[0]);
        }

        // roll right
        virtual double u2(double * o) = 0;

        // pitch forward
        virtual double u3(double * o) = 0;

        // yaw cw
        virtual double u4(double * o) = 0;

        // motor location for animation
        double * motorLocation(uint8_t i)
        {
            return &_motorLocations[3*i];
        }

        // motor direction for animation
        virtual int8_t motorDirection(uint8_t i)  = 0;

}; // class MultirotorFrame

