/*
* NoisySensor.cpp: Superclass for simulating sensors with Gaussian noise
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "NoisySensor.h"

NoisySensor::NoisySensor(uint8_t size, double mean, double stdev)
{
    _size  = size;

    _dist = std::normal_distribution<double>(mean, stdev);
}

NoisySensor::~NoisySensor()
{
}
 
void NoisySensor::addNoise(double vals[])
{
    for (uint8_t k=0; k<_size; ++k) {
        vals[k] += _dist(_generator);
    }
}
