/*
* GaussianNoise.h: Simulate Gaussian sensor noise
*
* XXX We should simulate ODR (output data rates) as well, but 
* UE4 frame rate is currently to slow to do that realistically.
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/


#include "GaussianNoise.h"

   
GaussianNoise::GaussianNoise(uint8_t size, float noise)
{
    _size  = size;
    _noise = noise;

    _dist = std::normal_distribution<float>(0, _noise);
}

GaussianNoise::~GaussianNoise()
{
}
 
void GaussianNoise::addNoise(float vals[])
{
    for (uint8_t k=0; k<_size; ++k) {
        vals[k] += _dist(_generator);
    }
}
