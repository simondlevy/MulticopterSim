/*
 * Abstract camera class for MulticopterSim using socket communication
 *
 * Copyright (C) 2021 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../MainModule/Camera.hpp"

class SocketCamera : public Camera {

    private:

        // Camera params
        static constexpr Resolution_t RES = RES_640x480;
        static constexpr float FOV = 135;

    public:

        SocketCamera()
            : Camera(FOV, RES)
        {
        }

    protected:

        virtual void processImageBytes(uint8_t * bytes) override
        { 
        }

}; // Class SocketCamera
