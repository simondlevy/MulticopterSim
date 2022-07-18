#define WIN32_LEAN_AND_MEAN

#define _USE_MATH_DEFINES
#include <cmath>

#include <core_rate.h>
#include <debug.h>
#include <hackflight.h>
#include <sensors.h>
#include <serial.h>
#include <pids/althold.h>

#include "FlightManager.hpp"

// Data shared between FlightManager and Rust ------------------

static hackflight_t _hf;
static Dynamics * _dyn;

// Rust stuff --------------------------------------------------

static uint32_t ALTIMETER_RATE = 100;
static uint32_t ALT_HOLD_RATE  = 50;

static void altimeterTask(void * hackflight, uint32_t usec)
{
    hackflight_t * hf = (hackflight_t *)hackflight;

    (void)usec;

    // NED => ENU
    hf->vstate.z  = -_dyn->x(Dynamics::STATE_Z);
    hf->vstate.dz = -_dyn->x(Dynamics::STATE_DZ);
}

static void checkTask(task_t * task, uint32_t usec)
{
    if (usec - task->desiredPeriodUs > task->lastExecutedAtUs) {
        task->fun(&_hf, usec);
        task->lastExecutedAtUs = usec;
    }
}

static void resetTask(task_t * task)
{
    task->lastExecutedAtUs = 0;
}

// FlightManager stuff ------------------------------------------------

FRustFlightManager::FRustFlightManager(
        APawn * pawn,
        Dynamics * dynamics,
        mixer_t mixer)
    : FFlightManager(dynamics)
{
    // Tuning constants for angle PID controller
    static const float RATE_P  = 1.4e0;
    static const float RATE_I  = 0;//1.9e+1;
    static const float RATE_D  = 2.1e-2;
    static const float RATE_F  = 0;//1.6e-2;
    static const float LEVEL_P = 50.0;

    static const float ALT_HOLD_KP = 0.75;
    static const float ALT_HOLD_KI = 1.5;

    // Reset last-time-executed for tasks
    resetTask(&_hf.rxTask);
    for (uint8_t k=0; k<_hf.sensorTaskCount; ++k) {
        resetTask(&_hf.sensorTasks[k]);
    }

    // Set up code in impl/ directory

    void shareMotors(float *);
    shareMotors(_motorvals);

    void shareDynamics(Dynamics *);
    shareDynamics(dynamics);

    static anglePidConstants_t anglePidConstants = {
        1.441305,     // Rate Kp
        19.55048,     // Rate Ki
        0.021160,     // Rate Kd
        0.0165048,    // Rate Kf
        0.0}; // 3.0, // Level Kp

    // Initialize Rust with angle PID tuning constants
    hackflightInit(&_hf, &anglePidConstants, mixer);

    // Simulate an altimeter
    hackflightAddSensor(&_hf, altimeterTask, ALTIMETER_RATE);

    // Add a PID controller for altitude hold
    altHoldPidInit(&_alt_pid, ALT_HOLD_KP, ALT_HOLD_KI,
            &_hf.rxAxes.demands.throttle);
    hackflightAddPidController(&_hf, altHoldPidUpdate, &_alt_pid);

    // Set instance variables
    _ready = true;
    _dyn = dynamics;
}

FRustFlightManager::~FRustFlightManager()
{
}

void FRustFlightManager::getMotors(double time, double* values)
{
    // Avoid null-pointer exceptions at startup, freeze after control
    // program halts
    if (!_ready) {
        return;
    }

    uint32_t usec = (uint32_t)(time * 1e6);

    static uint32_t _core_usec;

    // Sync core tasks to core period
    if (usec - _core_usec > CORE_PERIOD()) {
        _core_usec = usec;
        hackflightRunCoreTasks(&_hf);
    }

    // Poll "receiver" (joystick) periodcially
    checkTask(&_hf.rxTask, usec);

    // Check attitude task
    checkTask(&_hf.attitudeTask, usec);

    // Run sensors
    for (uint8_t k=0; k<_hf.sensorTaskCount; ++k) {
        checkTask(&_hf.sensorTasks[k], usec);
    }

    //  Get the new motor values
    for (uint8_t i=0; i < _actuatorCount; ++i) {
        values[i] = _motorvals[i];
    }
}

void FRustFlightManager::tick(void)
{
}
