/*
* SimBoard.h: Header for Hackflight::Board class implementation in MulticopterSim
*
* Copyright(C) 2018 Simon D.Levy
*
* MIT License
*/

#include <Runtime/Core/Public/Containers/Array.h>
#include <Runtime/Core/Public/Math/Quat.h>

#include <board.hpp>

#pragma once

class SimBoard : public hf::Board {

    private:

        // XXX Replace with FQuat and FVectors?
        float _elapsedTime;
        float _quat[4];
        float _gyro[3];
        float _motors[4];

    protected:

        bool getQuaternion(float quat[4]) override;

        bool getGyrometer(float gyro[3]) override;

        float getTime(void) override;

        void writeMotor(uint8_t index, float value) override;

    public:

        SimBoard();

        ~SimBoard();

        TArray<float> update(float deltaTime, FQuat quat, FVector gyro);
};

