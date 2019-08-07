/*
   DVS simulator demo program

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include "Davis346Sim.hpp"
#include "Davis346Display.hpp"

// meters, seconds
static constexpr double TARGET_SIZE     = 1; 
static constexpr double TARGET_DISTANCE = 10; 
static constexpr double TARGET_SPEED    = 0.5; 

int main(int argc, char ** argv)
{
    Davis346 sensor(TARGET_SIZE);

    Davis346Display display;

    while (true) {

        if (!display.displayEvents()) break;
    }

    return 0;
}


