#include <stdio.h>

#include "sockets/UdpServerSocket.hpp"
#include "sockets/UdpClientSocket.hpp"

static const char * HOST = "127.0.0.1";
static const short MOTOR_PORT = 5000;
static const short TELEM_PORT = 5001;

int main(int argc, char ** argv)
{
    UdpServerSocket motorServer = UdpServerSocket(MOTOR_PORT);
    UdpClientSocket telemClient = UdpClientSocket(HOST, TELEM_PORT);

    int count = 0;

    while (true) {

        double motorvals[4];

        motorServer.receiveData(motorvals, 32);

        double telem = 99;
        telemClient.sendData(&telem, sizeof(double));

        printf("%06d: %f %f %f %f\n", 
                count++, motorvals[0], motorvals[1], motorvals[2], motorvals[3], motorvals[4]);
    }

    motorServer.closeConnection();
    telemClient.closeConnection();

    return 0;
}
