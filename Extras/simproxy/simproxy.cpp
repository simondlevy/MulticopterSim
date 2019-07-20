/*
   UDP proxy for testing MulticopterSim socket comms

   Copyright(C) 2019 Simon D.Levy

   MIT License
 */

#include <stdio.h>

#include "../sockets/UdpServerSocket.hpp"
#include "../sockets/UdpClientSocket.hpp"
#include "../sockets/TwoWayUdp.hpp"

#include <dynamics/QuadXAP.hpp>

static const char * HOST           = "127.0.0.1";
static const short  MOTOR_PORT     = 5000;
static const short  TELEM_PORT     = 5001;
static const double DELTA_T        = 0.001;
static const uint32_t TIMEOUT_MSEC = 1000;

static MultirotorDynamics::params_t bigQuadParams = {

    // Dynamics: Amir's calculations
    5.30216718361085E-05,   // b
    2.23656692806239E-06,   // d
    16.47,                  // m
    0.6,                    // l
    2,                      // Ix
    2,                      // Iy
    3,                      // Iz
    3.08013E-04,            // Jr

    // maxrpm, estimated
    15000
};

int main(int argc, char ** argv)
{
    while (true) {

        TwoWayUdp twoWayUdp = TwoWayUdp(HOST, TELEM_PORT, MOTOR_PORT, TIMEOUT_MSEC);

        QuadXAPDynamics quad = QuadXAPDynamics(bigQuadParams);

        double time = 0;

        MultirotorDynamics::pose_t pose = {0};

        quad.init(pose);

        while (true) {

            double motorvals[4] = {0};

            if (!twoWayUdp.receive(motorvals, 32)) {
                printf("Timed out; restarting\n");
                break;
            }

            printf("%05f: %f %f %f %f\n", 
                    time, motorvals[0], motorvals[1], motorvals[2], motorvals[3]);

            quad.setMotors(motorvals);

            quad.update(DELTA_T);

            MultirotorDynamics::state_t state = {0};

            quad.getState(state);

            // Time Gyro, Quat, Location
            double telemetry[10] = {0};

            telemetry[0] = time;

            memcpy(&telemetry[1], &state.angularVel, 3*sizeof(double));
            memcpy(&telemetry[4], &state.bodyAccel, 3*sizeof(double));
            memcpy(&telemetry[7], &state.pose.location, 3*sizeof(double));

            twoWayUdp.send(telemetry, sizeof(telemetry));

            time += DELTA_T;
        }
    }

    return 0;
}
