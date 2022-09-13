/*
   UDP proxy for testing MulticopterSim socket comms

   Copyright(C) 2019 Simon D.Levy

   MIT License
 */

#include <stdio.h>
#include <stdio.h>
#include <stdint.h>

#include <core/pids/angle.h>
#include <core/pids/althold.h>
#include <core/mixers/fixedpitch/quadxbf.h>

#include "../../Simulator/Source/MultiSim/sockets/UdpClientSocket.hpp"
#include "../../Simulator/Source/MultiSim/sockets/UdpServerSocket.hpp"

// Comms
static const char * HOST = "127.0.0.1"; // localhost
static uint16_t  MOTOR_PORT = 5000;
static uint16_t  TELEM_PORT = 5001;

// PID constants

static const float RATE_P  = 1.441305;
static const float RATE_I  = 19.55048;
static const float RATE_D  = 0.021160;
static const float RATE_F  = 0.0165048;
static const float LEVEL_P = 0.0;

static const float ALT_HOLD_KP = 7.5e-2;
static const float ALT_HOLD_KI = 1.5e-1;

static float rad2deg(const double rad)
{
    return (float)(180 * rad / M_PI);
}

static VehicleState state_from_telemetry(const double telemetry[])
{
    return VehicleState( 
            (float)telemetry[1],     // x
            (float)telemetry[2],     // dx
            (float)telemetry[3],     // y
            (float)telemetry[4],     // dy
            -(float)telemetry[5],    // z  [NED => ENU]
            -(float)telemetry[6],    // dz [NED => ENU]
            rad2deg(telemetry[7]),   // phi
            rad2deg(telemetry[8]),   // dphi
            -rad2deg(telemetry[9]),  // theta  [note sign reversal]
            -rad2deg(telemetry[10]), // dtheta [note sign reversal]
            rad2deg(telemetry[11]),  // psi
            rad2deg(telemetry[12])   // dpsi
            );
}

static Demands demands_from_telemetry(const double telemetry[])
{
    constexpr float SCALE = 670;

    return Demands(
            (float)(telemetry[13] + 1) / 2, // [-1,+1] => [0,1]
            (float)telemetry[14] * SCALE,
            (float)telemetry[15] * SCALE,
            (float)telemetry[16] * SCALE
            );
}

int main(int argc, char ** argv)
{

    // Create sockets for telemetry in, motors out
    UdpServerSocket telemServer = UdpServerSocket(TELEM_PORT);
    UdpClientSocket motorClient = UdpClientSocket(HOST, MOTOR_PORT);

    // Create Hackflight objects

    static AnglePidController anglePid(
            RATE_P,
            RATE_I,
            RATE_D,
            RATE_F,
            LEVEL_P);

    static AltHoldPidController altHoldPid(
            ALT_HOLD_KP,
            ALT_HOLD_KI);

    static Mixer mixer = QuadXbfMixer::make();

    printf("Hit the Play button ... ");
    fflush(stdout);

    // Loop forever, waiting for clients
    while (true) {

        // Get incoming telemetry values
        double telemetry[17] = {};
        telemServer.receiveData(telemetry, sizeof(telemetry));

        // Sim sends negative time value on halt
        double time = telemetry[0];
        if (time < 0) {
            break;
        }

        // Convert simulator time to microseconds
        uint32_t usec = (uint32_t)(time * 1e6);

        // Build vehicle state 
        auto vstate = state_from_telemetry(telemetry);

        // Build demands
        auto demands = demands_from_telemetry(telemetry);

        // Reset PID controllers on zero throttle
        auto pidReset = demands.throttle < .05;

        // etl::vector<PidController *, 10> pidControllers =
        std::vector<PidController *> pidControllers = {&anglePid, &altHoldPid};

        // Run core Hackflight algorithm to get motor values
        auto motors = mixer.step(demands, vstate, &pidControllers, pidReset, usec);

        // Convert motor values to doubles
        double dmotorvals[4] = {
            motors.values[0],
            motors.values[1],
            motors.values[2],
            motors.values[3]
        };

        // Send back motor values
        motorClient.sendData(dmotorvals, sizeof(dmotorvals));

    } // while (true)

    return 0;
}
