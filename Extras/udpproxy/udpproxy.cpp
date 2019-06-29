/*
   UDP proxy for testing MulticopterSim socket comms

   Copyright(C) 2019 Simon D.Levy

   MIT License
 */

#include <stdio.h>

#include "sockets/UdpServerSocket.hpp"
#include "sockets/UdpClientSocket.hpp"

#include "MultirotorDynamics.hpp"

static const char * HOST       = "127.0.0.1";
static const short  MOTOR_PORT = 5000;
static const short  TELEM_PORT = 5001;
static const double DELTA_T    = 0.001;

int main(int argc, char ** argv)
{
    UdpServerSocket motorServer = UdpServerSocket(MOTOR_PORT);
    UdpClientSocket telemClient = UdpClientSocket(HOST, TELEM_PORT);

    int count = 0;

    double time = 0;

    while (true) {

        double motorvals[4] = {0};

        motorServer.receiveData(motorvals, 32);

        // Time Gyro, Quat, Location, Rotation
        double telemetry[14];

        telemetry[0] = time;

        telemClient.sendData(telemetry, sizeof(telemetry));

        printf("%06d: %f %f %f %f\n", 
                count++, motorvals[0], motorvals[1], motorvals[2], motorvals[3]);

        time += DELTA_T;
    }

    motorServer.closeConnection();
    telemClient.closeConnection();

    return 0;
}
