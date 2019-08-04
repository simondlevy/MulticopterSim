/*
   UDP proxy for testing MulticopterSim socket comms

   Copyright(C) 2019 Simon D.Levy

   MIT License
 */

#include <stdio.h>
#include "../sockets/TwoWayUdp.hpp"
#include <dynamics/QuadXAP.hpp>

static const char * HOST           = "127.0.0.1";
static const short  MOTOR_PORT     = 5000;
static const short  TELEM_PORT     = 5001;
static const double DELTA_T        = 0.001;

static MultirotorDynamics::Parameters params = MultirotorDynamics::Parameters(

        5.30216718361085E-05,   // b
        2.23656692806239E-06,   // d
        16.47,                  // m
        0.6,                    // l
        2,                      // Ix
        2,                      // Iy
        3,                      // Iz
        3.08013E-04,            // Jr
        15000                   // maxrpm
        );


int main(int argc, char ** argv)
{
    while (true) {

        TwoWayUdp twoWayUdp = TwoWayUdp(HOST, TELEM_PORT, MOTOR_PORT);

        QuadXAPDynamics quad = QuadXAPDynamics(&params);

        double time = 0;

        MultirotorDynamics::pose_t pose = {};

        quad.init(pose);

        while (true) {

            MultirotorDynamics::state_t state = quad.getState();

            // Time Gyro, Quat, Location
            double telemetry[10] = {0};

            telemetry[0] = time;

            memcpy(&telemetry[1], &state.angularVel, 3*sizeof(double));
            memcpy(&telemetry[4], &state.bodyAccel, 3*sizeof(double));
            memcpy(&telemetry[7], &state.pose.location, 3*sizeof(double));

            twoWayUdp.send(telemetry, sizeof(telemetry));

            double motorvals[4] = {};

            twoWayUdp.receive(motorvals, sizeof(motorvals));

            printf("t=%05f   m=%f %f %f %f  z=%+3.3f\n", 
                    time, motorvals[0], motorvals[1], motorvals[2], motorvals[3], state.pose.location[2]);

            quad.setMotors(motorvals);

            quad.update(DELTA_T);

            time += DELTA_T;
        }

    } while (true)

    return 0;
}
