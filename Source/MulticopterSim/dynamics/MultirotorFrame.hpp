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

        uint8_t   _motorCount = 0;

        double * _motorLocations = NULL; 

        double _l = 0;

        double sqr(double x)
        {
            return x*x;
        }

    protected:

        MultirotorFrame(const double * motorLocations, uint8_t motorCount) 
        {
            _motorCount = motorCount;

            _motorLocations = new double[3*motorCount];

            memcpy(_motorLocations, motorLocations, 3*motorCount*sizeof(double));

            // Pre-compute lever length for efficiency
            _l = sqrt(sqr(_motorLocations[0]) + sqr(_motorLocations[1]));

        }

        virtual ~MultirotorFrame(void)
        {
            delete _motorLocations;
        }

    public:

        typedef struct {

            double b;
            double d;
            double m;
            double Ix;
            double Iy;
            double Iz;
            double Jr;

            uint16_t maxrpm;

        } params_t;

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
            return _l;
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

