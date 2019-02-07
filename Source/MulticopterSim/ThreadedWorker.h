/*
 * ThreadedWorker.cpp: Threaded video code for  project
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "Core.h"
#include "Runnable.h"

/**
 * 
 */
class FThreadedWorker : public FRunnable {

private:

	FRunnableThread* _thread;

	bool _running;
	uint32_t _count;

public:

	FThreadedWorker();

	~FThreadedWorker();

	uint32_t getCount(void);

	// FRunnable interface.
	virtual bool Init();
	virtual uint32 Run();
	virtual void Stop();
};
