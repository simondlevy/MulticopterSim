/*
* NoisySensor.h: Superclass for simulating sensors with Gaussian noise
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/


#pragma once

#include <random>

/**
 * 
 */
class NoisySensor {

    private:

        std::default_random_engine _generator;
        std::normal_distribution<double> _dist;

        uint8_t _size;

    protected:

        NoisySensor(uint8_t size, double mean=0, double stdev=1);

        ~NoisySensor();

        void addNoise(double vals[]);
};
