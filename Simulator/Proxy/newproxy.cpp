/*
   UDP proxy for testing MulticopterSim socket comms

   Copyright(C) 2019 Simon D.Levy

   MIT License
 */

#include <stdio.h>
#include <stdio.h>
#include <stdint.h>

#include "../Source/MultiSim/sockets/TcpServerSocket.hpp"

// Comms
static const char * HOST = "127.0.0.1"; // localhost
static uint16_t  TELEM_PORT = 5000;

int main(int argc, char ** argv)
{

    // Use non-blocking socket
    TcpServerSocket telemServer = TcpServerSocket(HOST, TELEM_PORT, true);

    // Guards socket comms
    bool connected = false;

    uint32_t count = 0;

    // Loop forever, waiting for clients
    while (true) {

        printf("%08d: ", count++);

        if (connected) {
            printf("connected\n");
        }

        else {

            connected = telemServer.acceptConnection();

            printf("listening\n");

        }

    } // while (true)

    return 0;
}
