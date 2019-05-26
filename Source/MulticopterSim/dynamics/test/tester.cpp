#ifndef _WIN32

#include <MultirotorDynamics.hpp>

#include <stdio.h>

int main(int argc, char ** argv)
{
    double A[3][3] = {{4,5,6},{7,8,9},{10,11,12}};
    double x[3]    = {1,2,3};

    double y[3];

    MultirotorDynamics::dot(A, x, y);

    printf("%f %f %f\n", y[0], y[1], y[2]);
}
#endif
