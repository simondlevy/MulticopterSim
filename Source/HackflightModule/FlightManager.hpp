/*
   MulticopterSim FlightManager class implementation using UDP sockets 

   Acts as a client for a server program running on another address

   Sends joystick demands and vehicle state to server; receives motor
   values

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"

#include "SimReceiver.hpp"

class FHackflightFlightManager : public FFlightManager {

    private:

        // Joystick (RC transmitter, game controller) or keypad
        SimReceiver * _receiver = NULL;
        
        // Guards socket comms
        bool _ready = false;

    public:

        FHackflightFlightManager(APawn * pawn, Dynamics * dynamics)
            : FFlightManager(dynamics)
        {
            _receiver = new SimReceiver(pawn);

            _ready = true;
        }
		
        ~FHackflightFlightManager()
        {
        }

        virtual void getActuators(const double time, double * values) override
        {
            // Avoid null-pointer exceptions at startup, freeze after control
            // program halts
            if (!_ready) {
                return;
            }

            _receiver->poll();
        }

        void tick(void)
        {
        }

}; // FHackflightFlightManager
