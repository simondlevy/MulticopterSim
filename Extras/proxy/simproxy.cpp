/*
   UDP proxy for testing MulticopterSim socket comms

   Copyright(C) 2019 Simon D.Levy

   MIT License
 */

#include <stdio.h>
#include <stdio.h>
#include <stdint.h>

#include "../../Source/SocketModule/sockets/TwoWayUdp.hpp"
#include "../../Source/SocketModule/sockets/TcpClientSocket.hpp"

#include <dynamics/QuadXAP.hpp>

// Comms
static const char * HOST = "127.0.0.1"; // localhost
static uint16_t  MOTOR_PORT = 5000;
static uint16_t  TELEM_PORT = 5001;
static uint16_t  IMAGE_PORT = 5002;

// Image size
static uint16_t IMAGE_ROWS = 480;
static uint16_t IMAGE_COLS = 640;

// Time constant
static const double DELTA_T = 0.001;

static Dynamics::vehicle_params_t vparams = {

    // Estimated
    5.E-06, // b force constatnt [F=b*w^2]
    2.E-06, // d torque constant [T=d*w^2]

    // https://www.dji.com/phantom-4/info
    1.380,  // m mass [kg]

    // Estimated
    2,      // Ix [kg*m^2] 
    2,      // Iy [kg*m^2] 
    3,      // Iz [kg*m^2] 
    38E-04, // Jr prop inertial [kg*m^2] 

    0.350,  // l arm length [m]

    15000 // maxrpm
};


int main(int argc, char ** argv)
{
    // Allocate image bytes (rows * cols * rgba)
    uint8_t image[IMAGE_ROWS * IMAGE_COLS * 4];

    memset(image, 0, sizeof(image));

    // Create proxy image with diagonal red stripe
    for (uint16_t j=0; j<IMAGE_ROWS; ++j) {
        for (uint16_t k=0; k<IMAGE_COLS; ++k) {
            uint16_t l = (float)j/IMAGE_ROWS * IMAGE_COLS;
            image[(j*IMAGE_COLS+l)*4] = 255;
        }
    }

    // Loop forever, waiting for clients
    while (true) {

        // Create two-way comms for telemetry out, motors in
        TwoWayUdp twoWayUdp = TwoWayUdp(HOST, TELEM_PORT, MOTOR_PORT);

        // Create one-way server for images out
        TcpClientSocket imageSocket = TcpClientSocket(HOST, IMAGE_PORT);

        // Create quadcopter dynamics model
        QuadXAPDynamics dynamics = QuadXAPDynamics(vparams);

        // Set up initial conditions
        double time = 0;
        double rotation[3] = {0,0,0};
        dynamics.init(rotation);

        // Open image socket's connection to host
        imageSocket.openConnection();

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
            imageSocket.sendData(image, sizeof(image));

            // Get incoming motor values
            double motorvals[4] = {};
            twoWayUdp.receive(motorvals, sizeof(motorvals));

            printf("t=%05f   m=%f %f %f %f  z=%+3.3f\n", 
                    time, motorvals[0], motorvals[1], motorvals[2], motorvals[3], dynamics.x(Dynamics::STATE_Z));

            // Update dynamics with motor values
            dynamics.setMotors(motorvals);
            dynamics.update(DELTA_T);

            // Set AGL to arbitrary positive value to o avoid kinematic trick
            dynamics.setAgl(1);

            time += DELTA_T;
        }

    } // while (true)

    return 0;
}
