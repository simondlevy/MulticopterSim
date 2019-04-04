/*
* GaussianNoise.h: Simulate Gaussian sensor noise
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
class GaussianNoise {

    private:

        std::default_random_engine _generator;
        std::normal_distribution<double> _dist;

        uint8_t _size;

    public:

        GaussianNoise(uint8_t size, double mean=0, double stdev=1);

        ~GaussianNoise();

        void addNoise(double vals[]);

};
