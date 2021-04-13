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

// Estimated
static constexpr float b = 5.E-06; // force constatnt [F=b*w^2]
static constexpr float d = 2.E-06; // torque constant [T=d*w^2]

// https://www.dji.com/phantom-4/info
static constexpr float m = 1.380;  // mass [kg]

// Estimated
static constexpr float Ix = 2;      // [kg*m^2]
static constexpr float Iy = 2;      // [kg*m^2]
static constexpr float Iz = 3;      // [kg*m^2]
static constexpr float Jr = 38E-04; // prop inertial [kg*m^2]

static constexpr float l = 0.350;  // arm length [m]

static const uint16_t maxrpm = 15000; // maxrpm

int main(int argc, char ** argv)
{
    while (true) {

        TwoWayUdp twoWayUdp = TwoWayUdp(HOST, TELEM_PORT, MOTOR_PORT);

        QuadXAPDynamics dynamics = QuadXAPDynamics(b, d, m, Ix, Iy, Iz, Jr, l, maxrpm);

        double time = 0;

        double rotation[3] = {0,0,0};

        dynamics.init(rotation);

        while (true) {

            // Time + 12D state vector (Bouabdallah 2004)
            double telemetry[13] = {0};

            telemetry[0] = time;

            for (uint8_t k=0; k<12; ++k) {
                telemetry[k+1] = dynamics.x(k);
            }

            twoWayUdp.send(telemetry, sizeof(telemetry));

            double motorvals[4] = {};

            twoWayUdp.receive(motorvals, sizeof(motorvals));

            printf("t=%05f   m=%f %f %f %f  z=%+3.3f\n", 
                    time, motorvals[0], motorvals[1], motorvals[2], motorvals[3], dynamics.x(Dynamics::STATE_Z));

            dynamics.setMotors(motorvals);

            dynamics.update(DELTA_T);

            time += DELTA_T;
        }

    } while (true)

    return 0;
}
