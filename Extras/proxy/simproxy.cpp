/*
   UDP proxy for testing MulticopterSim socket comms

   Copyright(C) 2019 Simon D.Levy

   MIT License
 */

#include <stdio.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "../sockets/TcpClientSocket.hpp"

// Comms
static const char * HOST = "127.0.0.1"; // localhost
static uint16_t  PORT = 5001;

// Image size
static uint16_t ROWS = 480;
static uint16_t COLS = 640;

int main(int argc, char ** argv)
{
    // Allocate image bytes (rows * cols * rgba)
    uint8_t image[ROWS * COLS * 4];

    memset(image, 0, sizeof(image));

    for (uint16_t j=0; j<ROWS; ++j) {
        for (uint16_t k=0; k<COLS; ++k) {
            uint16_t l = (float)j/ROWS * COLS;
            image[(j*COLS+l)*4] = 255;
        }
    }

    // Loop forever, waiting for clients
    // Create one-way server for images out
    TcpClientSocket imageSocket = TcpClientSocket(HOST, PORT);

    imageSocket.openConnection();

    // Loop forever, communicating with client
    while (true) {

        imageSocket.sendData(image, sizeof(image));
    }

    return 0;
}
