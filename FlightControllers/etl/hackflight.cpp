/*
   UDP proxy for testing MulticopterSim socket comms

   Copyright(C) 2019 Simon D.Levy

   MIT License
 */

#define ETL_NO_PROFILE_HEADER

#include <stdio.h>
#include <stdio.h>
#include <stdint.h>

#include <hackflight_core.h>
#include <pids/althold.h>
#include <mixers/fixedpitch/quadxbf.h>

#include <Embedded_Template_Library.h>
#include <etl/vector.h>

#include "../../Simulator/Source/MultiSim/sockets/UdpClientSocket.hpp"
#include "../../Simulator/Source/MultiSim/sockets/UdpServerSocket.hpp"

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

static const float ALT_HOLD_KP = 7.5e-2;
static const float ALT_HOLD_KI = 1.5e-1;

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

    QuadXbfMixer mixer; 

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
        vehicle_state_t vstate = {
            (float)telemetry[1],
            (float)telemetry[2],
            (float)telemetry[3],
            (float)telemetry[4],
            (float)telemetry[5],
            (float)telemetry[6],
            (float)telemetry[7],
            (float)telemetry[8],
            (float)telemetry[9],
            (float)telemetry[10],
            (float)telemetry[11],
            (float)telemetry[12]
        };

        // Build demands
        demands_t demands = {
            (float)(telemetry[13] + 1) / 2, // [-1,+1] => [0,1]
            (float)telemetry[14] * 670,
            (float)telemetry[15] * 670,
            (float)telemetry[16] * 670
        };

        // Reset PID controllers on zero throttle
        auto pidReset = demands.throttle < .05;

        // Run core Hackflight algorithm to get motor values
        float motorvals[4] = {};
        HackflightCore::step(
                &demands,
                &vstate,
                &anglePid,
                pidReset,
                usec,
                &mixer,
                motorvals,
                &altHoldPid);

        // Convert motor values to doubles
        double dmotorvals[4] = {
            motorvals[0],
            motorvals[1],
            motorvals[2],
            motorvals[3]
        };

        // Send back motor values
        motorClient.sendData(dmotorvals, sizeof(dmotorvals));

    } // while (true)

    return 0;
}
