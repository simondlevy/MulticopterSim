/*
 * Threading support
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "Core.h"
#include "Runnable.h"

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

	FThreadedWorker(void);

	~FThreadedWorker();

    // Supports debugging on main thread
    void dbgprintf(const char * fmt, ...);
    const char * getMessage(void);

    static FThreadedWorker * stopThreadedWorker(FThreadedWorker * worker);

	// FRunnable interface.
	virtual bool Init();
	virtual uint32 Run();
	virtual void Stop();
};
