/*
   UDP proxy for testing MulticopterSim socket comms

   Copyright(C) 2019 Simon D.Levy

   MIT License
 */

#include <stdio.h>
#include <stdio.h>
#include <stdint.h>

#include <hackflight.h>
#include <mixers/fixedpitch/quadxbf.h>

#include "../../Source/MultiSim/sockets/UdpClientSocket.hpp"
#include "../../Source/MultiSim/sockets/UdpServerSocket.hpp"

// Comms
static const char * HOST = "127.0.0.1"; // localhost
static uint16_t  MOTOR_PORT = 5000;
static uint16_t  TELEM_PORT = 5001;

// PID constants

static const float RATE_P  = 1.4e0;
static const float RATE_I  = 0;//1.9e+1;
static const float RATE_D  = 2.1e-2;
static const float RATE_F  = 0;//1.6e-2;
static const float LEVEL_P = 50.0;

static const float ALT_HOLD_KP = 0.75;
static const float ALT_HOLD_KI = 1.5;

int main(int argc, char ** argv)
{

    // Create sockets for telemetry in, motors out
    UdpServerSocket telemServer = UdpServerSocket(TELEM_PORT);
    UdpClientSocket motorClient = UdpClientSocket(HOST, MOTOR_PORT);

    // Create Hackflight objects

    hackflight_t hf = {};

    anglePidConstants_t anglePidConstants = { 
        RATE_P,
        RATE_I,
        RATE_D,
        RATE_F,
        LEVEL_P
    };

    hackflightInit(&hf, &anglePidConstants, mixerQuadXbf);

    motor_config_t motorConfig = {
        0,     // disarmed
        1,     // high
        0,     // low
        false  // isDshot
    };

    printf("Hit the Play button ... ");
    fflush(stdout);

    // Loop forever, waiting for clients
    while (true) {

        // Get incoming telemetry values
        double telemetry[17] = {};
        telemServer.receiveData(telemetry, sizeof(telemetry));

        double time = telemetry[0];

        if (time < 0) {
            break;
        }

        // Convert simulator time to microseconds
        uint32_t usec = (uint32_t)(time * 1e6);

        // Build vehicle state 
        vehicle_state_t * vstate = &hf.vstate;
        vstate->x      = telemetry[1];
        vstate->dx     = telemetry[2];
        vstate->y      = telemetry[3];
        vstate->dy     = telemetry[4];
        vstate->z      = telemetry[5];
        vstate->dz     = telemetry[6];
        vstate->phi    = telemetry[7];
        vstate->dphi   = telemetry[8];
        vstate->theta  = telemetry[9];
        vstate->dtheta = telemetry[10];
        vstate->psi    = telemetry[11];
        vstate->dpsi   = telemetry[12];

        // Build demands
        demands_t * demands = &hf.demands;
        demands->throttle = (telemetry[13] + 1) / 2; // [-1,+1] => [0,1]
        demands->roll     = telemetry[14] * 670;
        demands->pitch    = telemetry[15] * 670;
        demands->yaw      = telemetry[16] * 670;

        // Run core Hackflight algorithm to get motor values
        float motorvals[4] = {};
        hackflightRunCoreTasks(
                &hf,
                usec,
                false, // no failsafe in sim
                &motorConfig,
                motorvals);

        // Convert motor values to doubles
        double dmotorvals[4] = {motorvals[0], motorvals[1], motorvals[2], motorvals[3]};

        // Send back motor values
        motorClient.sendData(dmotorvals, sizeof(dmotorvals));

    } // while (true)

    return 0;
}
