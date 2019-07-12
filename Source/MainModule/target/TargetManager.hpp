#pragma once

#include "ThreadedWorker.hpp"
#include "../../Extras/sockets/UdpServerSocket.hpp"

class FTargetManager : public FThreadedWorker {

public:

	FTargetManager() : FThreadedWorker()
	{

	}

	virtual void performTask(double currentTime) override
	{
		double gimbalvals[3] = { 0 };

		if (gimbalServer.receiveData(gimbalvals, 3 * sizeof(double))) {
			dbgprintf("%f %f %f", gimbalvals[0], gimbalvals[1], gimbalvals[2]);
		}
	}

private:

	UdpServerSocket gimbalServer = UdpServerSocket(7000, 1000);

};
