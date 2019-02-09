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
        float _elapsedTime = 0;
		FQuat _quat = FQuat(0,0,0,0);
		FVector _gyro = FVector(0, 0, 0);
		TArray<float> _motors = { 0,0,0,0 };

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

