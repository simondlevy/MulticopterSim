#include "FlightManager.hpp"
#include "../MainModule/Utils.hpp"

#include <SDL.h>

static const char * LIBRARY_FILENAME = "hackflight.dll";

FRustFlightManager::FRustFlightManager(APawn * pawn, Dynamics * dynamics)
    : FFlightManager(dynamics)
{
    _dynamics = dynamics;

    void * library_handle = SDL_LoadObject(LIBRARY_FILENAME);

    _run_hackflight = (run_hackflight_t) SDL_LoadFunction(library_handle, "c_run_hackflight");

    _joystick = new IJoystick();
}

FRustFlightManager::~FRustFlightManager()
{
}

bool FRustFlightManager::in_band(float value, float mid, float band) 
{
    return (value-mid) > -band && (value-mid) < band;
}

float FRustFlightManager::constrain_abs(float value, float limit) 
{
    return value < -limit ? -limit : value > limit ? limit : value;
}

void FRustFlightManager::run_alt_hold(
        float throttle,
        float z,
        float dz,
        alt_hold_t * alt_hold) 
{
    // Constants
    constexpr float kp = 7.5e-1;
    constexpr float ki = 1.5e0;
    constexpr float windup_max = 4.0e-1;
    constexpr float pilot_vel_z_max = 2.5e0;
    constexpr float stick_deadband = 2.0e-1;

    bool inband = in_band(throttle, 0.5, stick_deadband);

    debugline("throttle=%f  inband=%d\n", throttle, inband);

    float altitude = z;

    // Reset controller when moving into deadband
    float new_altitude_target =
        inband && !in_band(alt_hold->throttle_demand, 0.5, stick_deadband) ?
        altitude :
        alt_hold->altitude_target;

    // Inside deadband, target velocity is difference between altitude target and current
    // altitude; outside deadband, target velocity is proportional to stick demand
    float target_velocity =
        inband ?
        new_altitude_target - altitude :
        pilot_vel_z_max * throttle;

    // Compute error as altTarget velocity minus actual velocity, after
    // negating actual to accommodate NED
    float error = target_velocity + dz;

    // Accumualte error integral
    float new_error_integral = constrain_abs(alt_hold->error_integral + error, windup_max);

    // PI controller
    float new_throttle_demand =  kp * error + ki * new_error_integral;

    alt_hold->altitude_target = new_altitude_target;
    alt_hold->error_integral = new_error_integral;
    alt_hold->throttle_demand = new_throttle_demand;
}


void FRustFlightManager::run_hackflight(hackflight_t * hackflight)
{
    run_alt_hold(
        hackflight->demands.throttle, 
        -hackflight->vehicle_state.z,  // NED => ENU
        -hackflight->vehicle_state.dz, // NED => ENU
        &hackflight->alt_hold);

    float new_throttle = hackflight->alt_hold.throttle_demand;

    hackflight->motors.m1 = new_throttle;
    hackflight->motors.m2 = new_throttle;
    hackflight->motors.m3 = new_throttle;
    hackflight->motors.m4 = new_throttle;
}

void FRustFlightManager::getMotors(double time, double* values)
{
    (time);

    double joyvals[10] = {};

    _joystick->poll(joyvals);

    _hackflight.demands.throttle = joyvals[0] = (joyvals[0] + 1) / 2; // [-1,+1] => [0,1]
    _hackflight.demands.roll = joyvals[1];
    _hackflight.demands.pitch = joyvals[2];
    _hackflight.demands.yaw = joyvals[3];

    memcpy(&_hackflight.vehicle_state, &_dynamics->vstate, sizeof(Dynamics::vehicle_state_t));

    //auto hackflight = _run_hackflight(&demands, &_dynamics->vstate, &_alt_hold);
    //auto motors = hackflight.motors;
    // memcpy(&_alt_hold, &hackflight.alt_hold, sizeof(alt_hold_t));

    run_hackflight(&_hackflight);

    values[0] = _hackflight.motors.m1;
    values[1] = _hackflight.motors.m2;
    values[2] = _hackflight.motors.m3;
    values[3] = _hackflight.motors.m4;
}

void FRustFlightManager::tick(void)
{
}
