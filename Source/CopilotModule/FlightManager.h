/*

   Copyright(C) 2021 Simon D.Levy

   MIT License
*/

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"
#include "../MainModule/GameInput.hpp"

class FCopilotFlightManager : public FFlightManager {

    private:

        // Joystick (RC transmitter, game controller) or keypad
        GameInput * _gameInput = NULL;

	    // Time : State : Demands
        double _telemetry[17] = {};

        // Guards socket comms
        bool _connected = false;

    public:

        FCopilotFlightManager(APawn * pawn, Dynamics * dynamics);
		
        ~FCopilotFlightManager();

        virtual void getActuators(const double time, double * values) override;

        void tick(void);

}; // FCopilotFlightManager
