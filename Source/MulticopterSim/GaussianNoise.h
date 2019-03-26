/*
* GaussianNoise.h: Simulate Gaussian sensor noise
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/


#pragma once

#include "CoreMinimal.h"

#include <random>

/**
 * 
 */
class GaussianNoise {

    private:

        std::default_random_engine _generator;
        std::normal_distribution<float> _dist;

        uint8_t _size;
        float   _noise;

    public:

        GaussianNoise(uint8_t size, float noise);

        ~GaussianNoise();

        void addNoise(float vals[]);

};
