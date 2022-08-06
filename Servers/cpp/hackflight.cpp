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

#include "../../Source/SocketModule/sockets/UdpClientSocket.hpp"
#include "../../Source/SocketModule/sockets/UdpServerSocket.hpp"

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

    printf("Hit the Play button ... ");

    // Loop forever, waiting for clients
    while (true) {

        // Get incoming telemetry values
        double telemetry[17] = {};
        telemServer.receiveData(telemetry, sizeof(telemetry));

        // Convert simulator time to microseconds
        uint32_t usec = (uint32_t)(telemetry[0] * 1e6);

        // Build vehicle state 
        vehicle_state_t * vstate = &hf.vstate;
        vstate->x      = telemetry[0];
        vstate->dx     = telemetry[1];
        vstate->y      = telemetry[2];
        vstate->dy     = telemetry[3];
        vstate->z      = telemetry[4];
        vstate->dz     = telemetry[5];
        vstate->phi    = telemetry[6];
        vstate->dphi   = telemetry[7];
        vstate->theta  = telemetry[8];
        vstate->dtheta = telemetry[9];
        vstate->psi    = telemetry[10];
        vstate->dpsi   = telemetry[11];

        // Build demands
        demands_t * demands = &hf.demands;
        demands->throttle = telemetry[12];
        demands->roll     = telemetry[13];
        demands->pitch    = telemetry[14];
        demands->yaw      = telemetry[15];
        
        // Run core Hackflight algorithm to get motor values
        float motorvals[4];
        hackflightRunCore(
                &hf.demands,
                &hf.vstate,
                hf.pidControllers,
                hf.pidCount,
                hf.pidZeroThrottleItermReset,
                usec,
                hf.mixer,
                motorvals);

        // Send back motor values
        motorClient.sendData(motorvals, sizeof(motorvals));

    } // while (true)

    return 0;
}
