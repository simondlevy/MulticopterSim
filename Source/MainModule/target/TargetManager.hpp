#pragma once

#include "../MainModule/ThreadedWorker.hpp"

class FTargetManager : public FThreadedWorker {

protected:

	virtual void performTask(double currentTime) override
	{
		(void)currentTime;
		getPosition();
	}

	FTargetManager() : FThreadedWorker()
	{

	}

	virtual void getPosition(void) = 0;
};

// Factory method implemented by your subclass
//static FTargetManager * createTargetManager();
