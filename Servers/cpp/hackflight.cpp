/*
   UDP proxy for testing MulticopterSim socket comms

   Copyright(C) 2019 Simon D.Levy

   MIT License
 */

#include <stdio.h>
#include <stdio.h>
#include <stdint.h>

#include "../../Source/SocketModule/sockets/UdpClientSocket.hpp"
#include "../../Source/SocketModule/sockets/UdpServerSocket.hpp"

// Comms
static const char * HOST = "127.0.0.1"; // localhost
static uint16_t  MOTOR_PORT = 5000;
static uint16_t  TELEM_PORT = 5001;

// Time constant
static const double DELTA_T = 0.001;

int main(int argc, char ** argv)
{

    // Create sockets for telemetry in, motors out
    UdpClientSocket motorClient = UdpClientSocket(HOST, MOTOR_PORT);
    UdpServerSocket telemServer = UdpServerSocket(TELEM_PORT);


#if 0
    // Loop forever, waiting for clients
    while (true) {

        // Set up initial conditions
        double time = 0;

        // Loop forever, communicating with client
        while (true) {

            // To be sent to client
            double telemetry[17] = {0};

            // First value is time
            telemetry[0] = time;

            // Next 12 values are 12D state vector
            for (uint8_t k=0; k<12; ++k) {
                telemetry[k+1] = k;//dynamics.x(k);
            }

            // Last four values are receiver demands
            telemetry[13] = 0.1;
            telemetry[14] = 0.2;
            telemetry[15] = 0.3;
            telemetry[16] = 0.4;

            // Send telemetry data
            telemClient.sendData(telemetry, sizeof(telemetry));

            // Send image data
            //imageSocket.sendData(image, sizeof(image));

            // Get incoming motor values
            double motorvals[4] = {};
            motorServer.receiveData(motorvals, sizeof(motorvals));

            printf("t=%05f   m=%f %f %f %f  z=%+3.3f\n", 
                    time,
                    motorvals[0],
                    motorvals[1],
                    motorvals[2],
                    motorvals[3],
                    0.0); //dynamics.x(Dynamics::STATE_Z));

            time += DELTA_T;
        }

    } // while (true)
#endif

    return 0;
}
