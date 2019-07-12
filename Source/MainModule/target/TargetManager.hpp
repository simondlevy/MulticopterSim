#pragma once

#include "../MainModule/ThreadedWorker.hpp"

class FTargetManager : public FThreadedWorker {

public:

	FTargetManager();

protected:

    virtual void performTask(double currentTime) override;
};
