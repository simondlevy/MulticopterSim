#pragma once

#include "target/TargetManager.hpp"
#include "../../Extras/sockets/UdpServerSocket.hpp"


class FUdpTargetManger : public FTargetManager {

private:

	UdpServerSocket gimbalServer = UdpServerSocket(7000, 1000);

protected:

	void getPosition(void)
	{
		double gimbalvals[3] = { 0 };

		if (gimbalServer.receiveData(gimbalvals, 3 * sizeof(double))) {
			dbgprintf("%f %f %f", gimbalvals[0], gimbalvals[1], gimbalvals[2]);
		}
	}

};

FTargetManager * FTargetManager::create(void)
{
	return new FUdpTargetManger();
}
