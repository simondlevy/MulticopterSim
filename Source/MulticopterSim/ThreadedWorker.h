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

	FRunnableThread* _thread;

	bool _running;

protected:

    // Supports debugging on main thread
    char _message[200];

    virtual void performTask(void) = 0;

public:

	FThreadedWorker(void);

	~FThreadedWorker();

    const char * getMessage(void);

    static FThreadedWorker * stopThreadedWorker(FThreadedWorker * worker);

	// FRunnable interface.
	virtual bool Init();
	virtual uint32 Run();
	virtual void Stop();
};
