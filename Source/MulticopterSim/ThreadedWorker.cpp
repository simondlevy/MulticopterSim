/*
 * ThreadedWorker.cpp: Threaded video code for  project
 *
 * Adapted from https://wiki.unrealengine.com/Multi-Threading:_How_to_Create_Threads_in_UE4
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */


#include "ThreadedWorker.h"

FThreadedWorker::FThreadedWorker()
{
	_thread = FRunnableThread::Create(this, TEXT("FThreadedWorker"), 0, TPri_BelowNormal); //windows default = 8mb for thread, could specify more
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
	FPlatformProcess::Sleep(0.03);

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
