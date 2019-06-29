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

        double tmp = 0;
        telemClient.sendData(&tmp, sizeof(double));

        printf("%d\n", count++);
    }

    motorServer.closeConnection();
    telemClient.closeConnection();

    return 0;
}
