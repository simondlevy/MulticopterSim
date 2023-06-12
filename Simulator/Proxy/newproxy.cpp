/*
   Proxy for testing MulticopterSim socket comms

   Copyright(C) 2019 Simon D.Levy

   MIT License
 */

#include <stdio.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/time.h>

#include "../Source/MultiSim/sockets/TcpServerSocket.hpp"
#include "../Source/MultiSim/dynamics/fixedpitch/QuadXBF.hpp"

// Comms
static const char * HOST = "127.0.0.1"; // localhost
static uint16_t  TELEM_PORT = 5000;

// Time constant
static const double DELTA_T = 0.001;

static Dynamics::vehicle_params_t vparams = {

    // Estimated
    2.E-06, // d torque constant [T=d*w^2]

    // https://www.dji.com/phantom-4/info
    1.380,  // m mass [kg]

    // Estimated
    2,      // Ix [kg*m^2] 
    2,      // Iy [kg*m^2] 
    3,      // Iz [kg*m^2] 
    38E-04, // Jr prop inertial [kg*m^2] 

    15000 // maxrpm
};

static FixedPitchDynamics::fixed_pitch_params_t fparams = {

    // Estimated
    5.E-06, // b force constatnt [F=b*w^2]
    0.350   // l arm length [m]
};

static double get_current_time(void)
{
    struct timeval tv = {};
    gettimeofday(&tv,NULL);
    return tv.tv_sec + tv.tv_usec / 1e6;
}

int main(int argc, char ** argv)
{
    TcpServerSocket server = TcpServerSocket(HOST, TELEM_PORT);

    // Guards socket comms
    bool connected = false;

    // Create quadcopter dynamics model
    QuadXBFDynamics dynamics =
        QuadXBFDynamics(vparams, fparams, false); // no auto-land

    // Set up initial conditions
    double rotation[3] = {0,0,0};
    dynamics.init(rotation);

    printf("Listening for client on %s:%d \n", HOST, TELEM_PORT);

    auto tstart = get_current_time();

    // Loop forever, waiting for clients
    while (true) {

        if (connected) {

            static bool first;

            if (!first) {
                printf("Connected\n");
                first = true;
            }

            double telemetry[2] = {

                get_current_time() - tstart,

                dynamics.vstate.x
            };

            server.sendData(telemetry, sizeof(telemetry));

            /*

            // Next 12 values are 12D state vector
            telemetry[2] = dynamics.vstate.dx;
            telemetry[3] = dynamics.vstate.y;
            telemetry[4] = dynamics.vstate.dy;
            telemetry[5] = dynamics.vstate.z;
            telemetry[6] = dynamics.vstate.dz;
            telemetry[7] = dynamics.vstate.phi;
            telemetry[8] = dynamics.vstate.dphi;
            telemetry[9] = dynamics.vstate.theta;
            telemetry[10] = dynamics.vstate.dtheta;
            telemetry[11] = dynamics.vstate.psi;
            telemetry[12] = dynamics.vstate.dpsi;

            // Last four values are receiver demands
            telemetry[13] = 0.1;
            telemetry[14] = 0.2;
            telemetry[15] = 0.3;
            telemetry[16] = 0.4;

            // Send telemetry data
            server.sendData(telemetry, sizeof(telemetry));

            // Get incoming motor values
            double motorvals[4] = {};
            server.receiveData(motorvals, sizeof(motorvals));

            printf("t=%05f   m=%f %f %f %f  z=%+3.3f\n", 
            time,
            motorvals[0],
            motorvals[1],
            motorvals[2],
            motorvals[3],
            dynamics.vstate.z);

            float dvals[4] = {motorvals[0], motorvals[1], motorvals[2], motorvals[3]};

            // Update dynamics with motor values
            dynamics.update(dvals, DELTA_T);
             */

            // Set AGL to arbitrary positive value to avoid kinematic trick
            dynamics.setAgl(1);
        }

        else {

            connected = server.acceptConnection();

        }

    } // while (true)

    return 0;
}
