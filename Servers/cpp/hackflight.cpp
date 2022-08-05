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
    UdpServerSocket telemServer = UdpServerSocket(TELEM_PORT);
    UdpClientSocket motorClient = UdpClientSocket(HOST, MOTOR_PORT);

    printf("Hit the Play button ... ");

    // Loop forever, waiting for clients
    while (true) {

        // Get incoming telemetry values
        double telemetry[17] = {};
        telemServer.receiveData(telemetry, sizeof(telemetry));

        // XXX process values

        // Send back motor values
        double motorvals[4] = {0.6, 0.6, 0.6, 0.6};
        motorClient.sendData(motorvals, sizeof(motorvals));

    } // while (true)

    return 0;
}
