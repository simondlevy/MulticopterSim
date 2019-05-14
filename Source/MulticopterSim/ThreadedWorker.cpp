/*
 * ThreadedWorker.cpp: Threading support
 *
 * Adapted from https://wiki.unrealengine.com/Multi-Threading:_How_to_Create_Threads_in_UE4
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */


#include "ThreadedWorker.h"

/*
FThreadedWorker::FThreadedWorker(AVehiclePawn * vehiclePawn)
{
	_thread = FRunnableThread::Create(this, TEXT("FThreadedWorker"), 0, TPri_BelowNormal); 

    _vehiclePawn = vehiclePawn;

    *_message = 0;
}

FThreadedWorker::~FThreadedWorker()
{
	delete _thread;
}

bool FThreadedWorker::Init()
{
	_running = false;

	return true;
}

uint32_t FThreadedWorker::Run()
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

void FThreadedWorker::Stop()
{
	_running = false;

	// Final wait after stopping
	FPlatformProcess::Sleep(0.03);
}

double FThreadedWorker::getCurrentTime(void)
{
	return _vehiclePawn->getCurrentTime();
}

const char * FThreadedWorker::getMessage(void)
{
    return (const char *)_message;
}

FThreadedWorker * FThreadedWorker::stopThreadedWorker(FThreadedWorker * worker)
{
    worker->Stop();
    delete worker;
    return (FThreadedWorker *)NULL;
}

*/
