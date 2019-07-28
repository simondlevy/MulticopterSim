#pragma once

#include "../MainModule/ThreadedWorker.hpp"

class FTargetManager : public FThreadedWorker {

protected:

	FVector _location;

	virtual void performTask(double currentTime) override
	{
		computeLocation(currentTime);
	}

	FTargetManager() : FThreadedWorker()
	{
		_location = FVector(0, 10, 0);
	}

	virtual void computeLocation(double currentTime) = 0;

public:

	const FVector & getLocation(void)
	{
		return _location;
	}
};
