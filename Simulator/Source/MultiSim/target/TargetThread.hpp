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

#include "../sockets/UdpServerSocket.hpp"

#include "Runtime/Core/Public/HAL/Runnable.h"

class FTargetThread : public FRunnable {

    private:

        FRunnableThread * _thread = NULL;

        bool _running = false;

        // Socket comms
        UdpServerSocket * _kinematicsServer = NULL;

        // Vehicle pawn
        APawn * _pawn = NULL;

    public:

        // Constructor, called main thread
        FTargetThread(
                APawn * pawn,
                const char * host="127.0.0.1",
                const short port=5003)
        {
            _pawn = pawn;

            _thread =
                FRunnableThread::Create(
                        this, TEXT("FThreadedManage"), 0, TPri_BelowNormal);
        }

        ~FTargetThread(void)
        {
            // Close sockets
            UdpServerSocket::free(_kinematicsServer);

           delete _thread;
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
