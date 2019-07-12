#pragma once

#include "target/TargetManager.hpp"
#include "../../Extras/sockets/UdpServerSocket.hpp"

static	UdpServerSocket gimbalServer = UdpServerSocket(7000, 1000);

FTargetManager::FTargetManager() : FThreadedWorker()
{

}

void FTargetManager::performTask(double currentTime)
{
    double gimbalvals[3] = { 0 };

    if (gimbalServer.receiveData(gimbalvals, 3 * sizeof(double))) {
    	dbgprintf("%f %f %f", gimbalvals[0], gimbalvals[1], gimbalvals[2]);
    }
}
