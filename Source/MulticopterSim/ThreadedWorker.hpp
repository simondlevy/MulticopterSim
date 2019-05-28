/*
 * Threading support for MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "Core.h"
#include "Runnable.h"

#include <stdarg.h>

class FThreadedWorker : public FRunnable {

    private:

        FRunnableThread * _thread = NULL;

        bool _running = false;

        // Supports debugging on main thread
        static const uint16_t MAXMSG = 1000;
        char _message[MAXMSG] = {0};

    protected:

        // Implemented differently by each subclass
        virtual void performTask(void) = 0;

    public:

        FThreadedWorker(void)
        {
            _thread = FRunnableThread::Create(this, TEXT("FThreadedWorker"), 0, TPri_BelowNormal); 

            *_message = 0;
        }


        ~FThreadedWorker()
        {
            delete _thread;
        }


        // Supports debugging on main thread
        void dbgprintf(const char * fmt, ...)
        {
            va_list ap;
            va_start(ap, fmt);
            vsnprintf(_message, MAXMSG, fmt, ap);
            va_end(ap);
        }

        const char * getMessage(void)
        {
            return (const char *)_message;
        }

        static FThreadedWorker * stopThreadedWorker(FThreadedWorker * worker)
        {
            worker->Stop();
            delete worker;
            return (FThreadedWorker *)NULL;
        }

        // FRunnable interface.

        virtual bool Init() override
        {
            _running = false;

            return true;
        }

        virtual uint32_t Run() override
        {
            // Initial wait before starting
            FPlatformProcess::Sleep(0.5);

            _running = true;

            while (_running) {
                performTask();
                FPlatformProcess::Sleep(.0005); // Wait a bit to allow other threads to run
            }

            return 0;
        }

        virtual void Stop() override
        {
            _running = false;

            // Final wait after stopping
            FPlatformProcess::Sleep(0.03);
        }

}; // class FThreadedWorker
