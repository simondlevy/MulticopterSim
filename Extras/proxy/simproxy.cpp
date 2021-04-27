/*
   UDP proxy for testing MulticopterSim socket comms

   Copyright(C) 2019 Simon D.Levy

   MIT License
 */

#include <stdio.h>
#include <stdio.h>
#include <stdint.h>

#include "../sockets/TwoWayUdp.hpp"

#include <dynamics/QuadXAP.hpp>

// Comms
static const char * HOST = "127.0.0.1"; // localhost
static uint16_t  MOTOR_PORT = 5000;
static uint16_t  TELEM_PORT = 5001;
static uint16_t  IMAGE_PORT = 5002;

// Image size
static uint16_t IMAGE_ROWS = 480;
static uint16_t IMAGE_COLS = 640;
static uint16_t IMAGE_STRIP_HEIGHT = 20;

// Time constant
static const double DELTA_T    = 0.001;

// Estimated
static constexpr float b = 5.E-06; // force constatnt [F=b*w^2]
static constexpr float d = 2.E-06; // torque constant [T=d*w^2]

// https://www.dji.com/phantom-4/info
static constexpr float m = 1.380;  // mass [kg]

// Estimated vehicle properties
static constexpr float Ix = 2;        // [kg*m^2]
static constexpr float Iy = 2;        // [kg*m^2]
static constexpr float Iz = 3;        // [kg*m^2]
static constexpr float Jr = 38E-04;   // prop inertial [kg*m^2]
static constexpr float l = 0.350;     // arm length [m]
static const uint16_t maxrpm = 15000; 

int main(int argc, char ** argv)
{
    // Allocate image bytes (rows * cols * rgba)
    uint8_t image[IMAGE_ROWS * IMAGE_COLS * 4];

    memset(image, 0, sizeof(image));

    uint16_t strips = IMAGE_ROWS / IMAGE_STRIP_HEIGHT;
    uint16_t stripsize = IMAGE_COLS * IMAGE_STRIP_HEIGHT * 4;

    // Loop forever, waiting for clients
    while (true) {

        // Create two-way comms for telemetry out, motors in
        TwoWayUdp twoWayUdp = TwoWayUdp(HOST, TELEM_PORT, MOTOR_PORT);

        // Create one-way server for images out
        UdpClientSocket imageUdp = UdpClientSocket(HOST, IMAGE_PORT);

        // Create quadcopter dynamics model
        QuadXAPDynamics dynamics = QuadXAPDynamics(b, d, m, Ix, Iy, Iz, Jr, l, maxrpm);

        // Set up initial conditions
        double time = 0;
        double rotation[3] = {0,0,0};
        dynamics.init(rotation);

        // Loop forever, communicating with client
        while (true) {

            // Time + 12D state vector (Bouabdallah 2004)
            double telemetry[13] = {0};

            // Fill outgoing telemetry data
            telemetry[0] = time;
            for (uint8_t k=0; k<12; ++k) {
                telemetry[k+1] = dynamics.x(k);
            }

            // Send telemetry data
            twoWayUdp.send(telemetry, sizeof(telemetry));

            // Send image data
            for (uint16_t k=0; k<strips; ++k) {

                imageUdp.sendData(&image[k*stripsize], stripsize);
            }
             // Get incoming motor values
            double motorvals[4] = {};
            twoWayUdp.receive(motorvals, sizeof(motorvals));

            printf("t=%05f   m=%f %f %f %f  z=%+3.3f\n", 
                    time, motorvals[0], motorvals[1], motorvals[2], motorvals[3], dynamics.x(Dynamics::STATE_Z));

            // Update dynamics with motor values
            dynamics.setMotors(motorvals);
            dynamics.update(DELTA_T);

            time += DELTA_T;
        }

    } // while (true)

    return 0;
}
