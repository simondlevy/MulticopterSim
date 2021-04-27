/*
 * Abstract camera class for MulticopterSim using socket communication
 *
 * Copyright (C) 2021 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../MainModule/Camera.hpp"

#include "../../Extras/sockets/TcpClientSocket.hpp"

class SocketCamera : public Camera {

    private:

        // Comms
        static constexpr char * HOST = "127.0.0.1"; // localhost
        static constexpr uint16_t PORT = 5002;

        // Camera params
        static constexpr Resolution_t RES = RES_640x480;
        static constexpr float FOV = 135;
        static constexpr uint16_t ROWS = 480;
        static constexpr uint16_t COLS = 640;
        static constexpr uint16_t STRIP_HEIGHT = 20;

        // Create one-way TCP socket server for images out
        TcpClientSocket imageSocket = TcpClientSocket(HOST, PORT);

        // XXX for testing
        uint8_t image[ROWS * COLS * 4];

    public:

        SocketCamera()
            : Camera(FOV, RES)
        {
            // Open image socket's connection to host
            imageSocket.openConnection();

            // Create proxy image with diagonal red stripe
            for (uint16_t j=0; j<ROWS; ++j) {
                for (uint16_t k=0; k<COLS; ++k) {
                    uint16_t l = (float)j/ROWS * COLS;
                    image[(j*COLS+l)*4] = 255;
                }
            }

        }

    protected:

        virtual void processImageBytes(uint8_t * bytes) override
        { 
            // Send image data
            imageSocket.sendData(image, sizeof(image));
        }

}; // Class SocketCamera
