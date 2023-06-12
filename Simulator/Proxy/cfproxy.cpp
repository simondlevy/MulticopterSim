/*
   Proxy for testing MulticopterSim CrazyFlie comms

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

static void delay(const double t)
{
    auto tstart = get_current_time();

    while (get_current_time() - tstart < t) {
    }
}

int main(int argc, char ** argv)
{
    TcpServerSocket server = TcpServerSocket(HOST, TELEM_PORT, true);

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

            static bool was_connected;
            if (!was_connected) {
                printf("Connected\n");
                fflush(stdout);
                was_connected = true;
            }

            const double time = get_current_time() - tstart;

            const double telemetry[] = {

                // time
                time,

                // vehicle state
                dynamics.vstate.x,
                dynamics.vstate.dx,
                dynamics.vstate.y,
                dynamics.vstate.dy,
                dynamics.vstate.z,
                dynamics.vstate.dz,
                dynamics.vstate.phi,
                dynamics.vstate.dphi,
                dynamics.vstate.theta,
                dynamics.vstate.dtheta,
                dynamics.vstate.psi,
                dynamics.vstate.dpsi,

                // stick values
                0.1,
                0.2,
                0.3,
                0.4
            };

            server.sendData((void *)telemetry, sizeof(telemetry));

            delay(1e-4);

            /*

            // Get incoming motor values
            float motorvals[4] = {};
            server.receiveData(motorvals, sizeof(motorvals));

            printf("t=%05f   m=%f %f %f %f  z=%+3.3f\n", 
            time,
            motorvals[0],
            motorvals[1],
            motorvals[2],
            motorvals[3],
            dynamics.vstate.z);

            float dvals[] = {
                (float)motorvals[0],
                (float)motorvals[1],
                (float)motorvals[2],
                (float)motorvals[3]
            };

            // Update dynamics with motor values
            dynamics.update(dvals, DELTA_T);

            // Set AGL to arbitrary positive value to avoid kinematic trick
            dynamics.setAgl(1);
            */
        }

        else {

            connected = server.acceptConnection();

        }

    } // while (true)

    return 0;
}
