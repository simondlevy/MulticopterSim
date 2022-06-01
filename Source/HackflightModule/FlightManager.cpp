#define WIN32_LEAN_AND_MEAN

#define _USE_MATH_DEFINES
#include <cmath>

#include <debug.h>
#include <hackflight.h>

#include "FlightManager.hpp"

FHackflightFlightManager::FHackflightFlightManager(APawn * pawn, Dynamics * dynamics)
    : FFlightManager(dynamics)
{
    // Set up code in impl/ directory

    void shareMotors(float *);
    shareMotors(_motorvals);

    void shareDynamics(Dynamics *);
    shareDynamics(dynamics);

    // Interact with Hackflight

    hackflightInit();

    // Set instance variables
    _ready = true;
    _dynamics = dynamics;
}

FHackflightFlightManager::~FHackflightFlightManager()
{
}

static void checkTask(task_t * task, uint32_t usec)
{
    if (usec - task->desiredPeriodUs > task->lastExecutedAtUs) {
        task->fun(usec);
        task->lastExecutedAtUs = usec;
    }
}

// Caled from fast thread
void FHackflightFlightManager::getActuators(const double time, double * values)
{
    // Avoid null-pointer exceptions at startup, freeze after control
    // program halts
    if (!_ready) {
        return;
    }

    uint32_t usec = (uint32_t)(time * 1e6);

    static uint32_t _core_usec;

    // Sync core tasks to gyro period
    if (usec - _core_usec > GYRO_PERIOD()) {
        _core_usec = usec;
        hackflightRunCoreTasks();
    }

    // Poll "receiver" (joystick) periodcially
    if (usec - _rxTask.desiredPeriodUs > _rxTask.lastExecutedAtUs) {
        _rxTask.fun(usec);
        _rxTask.lastExecutedAtUs = usec;
    }

    //debugPrintf("m1=%+3.3f    m2=%+3.3f    m3=%+3.3f    m4=%+3.3f",    
    //        _motorvals[0], _motorvals[1], _motorvals[2], _motorvals[3]);

    //  Get the new motor values
    for (uint8_t i=0; i < _actuatorCount; ++i) {
        values[i] = _motorvals[i];
    }
}

void FHackflightFlightManager::tick(void)
{
}
