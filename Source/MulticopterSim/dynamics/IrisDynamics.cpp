/*
 * MultirotorDynamics constants for 3DR Iris quadcopter
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "QuadXAPDynamics.hpp"

class IrisDynamics : public QuadXAPDynamics {

    protected:

        // From http://www.arducopter.co.uk/iris-quadcopter-uav.html
        const double m(void)  override  { return 1.280; }     // kg
        const double l(void)  override  { return 0.225; }     // m

        // Estimated
        
        const double b(void)  override  { return 5.302e-6; }
        const double d(void)  override  { return 2.2375-6; }
        const double Ix(void) override  { return 2.000; }
        const double Iy(void) override  { return 2.000; }
        const double Iz(void) override  { return 3.000; }
        const double Jr(void) override  { return 3.080e-4; }  // Kg*m^2

        const unsigned int maxrpm(void) override { return 10000; }
}; 

// Factory method
MultirotorDynamics * MultirotorDynamics::create(void)
{
    return new IrisDynamics();
}

