/*
 * ThreadedWorker.h: Threading support
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

/*
#include "Core.h"
#include "Runnable.h"
#include "VehiclePawn.h"
*/

class FThreadedWorker /*: public FRunnable*/ {

    /*
private:

	FRunnableThread* _thread;

    AVehiclePawn * _vehiclePawn; // for getCurrentTime()

	bool _running;

protected:

    virtual void performTask(void) = 0;

	double getCurrentTime(void);

    // Supports debugging on main thread
    char _message[200];

public:

	FThreadedWorker(class AVehiclePawn * vehiclePawn);

	~FThreadedWorker();

    const char * getMessage(void);

    static FThreadedWorker * stopThreadedWorker(FThreadedWorker * worker);

	// FRunnable interface.
	virtual bool Init();
	virtual uint32 Run();
	virtual void Stop();
    */
};
