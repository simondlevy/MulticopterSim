/*
 * Abstract camera class for MulticopterSim using socket communication
 *
 * Copyright (C) 2021 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../MainModule/Camera.hpp"

#include "sockets/TcpClientSocket.hpp"

class SocketCamera : public Camera {

    private:

        // Comms
        static constexpr char * HOST = "127.0.0.1"; // localhost
        static constexpr uint16_t PORT = 5002;

        // Camera params
        static constexpr Resolution_t RES = RES_640x480;
        static constexpr float FOV = 135;

        // Create one-way TCP socket server for images out
        TcpClientSocket imageSocket = TcpClientSocket(HOST, PORT);

    public:

        SocketCamera(float x=Camera::X, float y=Camera::Y, float z=Camera::Z)
            : Camera(FOV, RES, x, y, z)
        {
            // Open image socket's connection to host
            imageSocket.openConnection();
        }

    protected:

        virtual void processImageBytes(uint8_t * bytes) override
        { 
            // Send image data
            imageSocket.sendData(bytes, _rows*_cols*4);
        }

}; // Class SocketCamera
