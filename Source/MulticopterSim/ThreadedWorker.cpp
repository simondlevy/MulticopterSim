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
#include "VehiclePawn.h"


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
	_count = 0;
	_running = false;

	return true;
}

uint32_t FThreadedWorker::Run()
{
	// Initial wait before starting
	FPlatformProcess::Sleep(0.03);

	_running = true;

	while (_running) {
		_count++;
		FPlatformProcess::Sleep(.0005); // Wait a bit to allow other threads to run
	}

	return 0;
}

void FThreadedWorker::Stop()
{
	_running = false;
	_count = 0;
}

uint32_t FThreadedWorker::getCount(void)
{
	return _count;
}


