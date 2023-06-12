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

    // Loop forever, waiting for clients
    while (true) {

        if (connected) {

            static bool was_connected;
            if (!was_connected) {
                printf("Connected\n");
                fflush(stdout);
                was_connected = true;
            }

            static double fake_z;

            fake_z += .001;

            const double telemetry[] = {

                // vehicle state
                dynamics.vstate.x,
                dynamics.vstate.y,
                fake_z /*dynamics.vstate.z*/ ,
                dynamics.vstate.phi,
                dynamics.vstate.theta,
                dynamics.vstate.psi
            };

            server.sendData((void *)telemetry, sizeof(telemetry));

            double sticks[4] = {};
            server.receiveData(sticks, sizeof(sticks));

            printf("t=%3.3f  r=%+3.3f  p=%+3.3f  y=%+3.3f\n",
                    sticks[0], sticks[1], sticks[2], sticks[3]);

            // Update dynamics with motor values
            // dynamics.update(motorvals, DELTA_T);


            // Set AGL to arbitrary positive value to avoid kinematic trick
            dynamics.setAgl(1);
        }

        else {

            connected = server.acceptConnection();

        }

    } // while (true)

    return 0;
}