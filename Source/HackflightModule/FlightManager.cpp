#define WIN32_LEAN_AND_MEAN

#define _USE_MATH_DEFINES
#include <cmath>

#include <debug.h>
#include <hackflight.h>
#include <pids/althold.h>

#include "FlightManager.hpp"

// Dynamics shared between FlightManager and Hackflight --------------

static Dynamics * _dyn;

// Hackflight stuff --------------------------------------------------

static uint32_t ALTIMETER_RATE = 100;
static uint32_t ALT_HOLD_RATE  = 50;

static void altimeter(uint32_t usec)
{
    (void)usec;
    _state.z = _dyn->x(Dynamics::STATE_Z);
}

static void checkTask(task_t * task, uint32_t usec)
{
    if (usec - task->desiredPeriodUs > task->lastExecutedAtUs) {
        task->fun(usec);
        task->lastExecutedAtUs = usec;
    }
}

// FlightManager stuff ------------------------------------------------

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
    hackflightAddSensor(altimeter, ALTIMETER_RATE);
    hackflightAddPidController(altHoldPidUpdate, &_rx_axes.demands.throttle);

    // Set instance variables
    _ready = true;
    _dyn = dynamics;
}

FHackflightFlightManager::~FHackflightFlightManager()
{
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
    checkTask(&_rxTask, usec);

    // Run sensors
    for (uint8_t k=0; k<_sensor_task_count; ++k) {
        checkTask(&_sensor_tasks[k], usec);
    }

    //  Get the new motor values
    for (uint8_t i=0; i < _actuatorCount; ++i) {
        values[i] = _motorvals[i];
    }
}

void FHackflightFlightManager::tick(void)
{
}
