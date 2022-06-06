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

    // NED => ENU
    _state.z  = -_dyn->x(Dynamics::STATE_Z);
    _state.dz = -_dyn->x(Dynamics::STATE_DZ);
}

static void checkTask(task_t * task, uint32_t usec)
{
    if (usec - task->desiredPeriodUs > task->lastExecutedAtUs) {
        task->fun(usec);
        task->lastExecutedAtUs = usec;
    }
}

static void resetTask(task_t * task)
{
    task->lastExecutedAtUs = 0;
}

// FlightManager stuff ------------------------------------------------

FHackflightFlightManager::FHackflightFlightManager(APawn * pawn, Dynamics * dynamics)
    : FFlightManager(dynamics)
{
    // Tuning constants for angle PID controller
    static const float RATE_P  = 1.441305;
    static const float RATE_I  = 19.55048;
    static const float RATE_D  = 0.021160;
    static const float RATE_F  = 0.0165048;
    static const float LEVEL_P = 0 /*3.0*/;

    // Reset last-time-executed for tasks
    resetTask(&_rxTask);
    for (uint8_t k=0; k<_sensor_task_count; ++k) {
        resetTask(&_sensor_tasks[k]);
    }

    // Set up code in impl/ directory

    void shareMotors(float *);
    shareMotors(_motorvals);

    void shareDynamics(Dynamics *);
    shareDynamics(dynamics);

    // Initialize Hackflight with angle PID tuning constants
    hackflightInit(RATE_P, RATE_I, RATE_D, RATE_F, LEVEL_P);

    // Simulate an altimeter and add a PID controller to use it for altitude hold
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

    // Check attitude task
    checkTask(&_attitudeTask, usec);

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
