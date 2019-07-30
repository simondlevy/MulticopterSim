#pragma once

#include "../MainModule/ThreadedManager.hpp"

class FTargetManager : public FThreadedManager {

protected:

	FVector _location;

	virtual void performTask(double currentTime) override
	{
		computeLocation(currentTime);
	}

	FTargetManager() : FThreadedManager()
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
