/*
 * Threaded target-controller class for MultiSim
 *
 * Gets instantiated in Target::beginPlay()
 *
 * Copyright (C) 2023 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#define WIN32_LEAN_AND_MEAN

#include "Runtime/Core/Public/HAL/Runnable.h"

class FTargetThread : public FRunnable {

    public:

        // Constructor, called main thread
        FTargetThread(
                APawn * pawn,
                const char * host="127.0.0.1",
                const short port=5000)
        {
        }

        ~FTargetThread(void)
        {
        }

        virtual bool Init() override
        {
            return FRunnable::Init();
        }

        virtual uint32_t Run() override
        {
            return 0;
        }

        virtual void Stop() override
        {
            FRunnable::Stop();
        }

}; // class FTargetThread
