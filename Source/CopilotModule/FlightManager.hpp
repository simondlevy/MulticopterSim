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

        FCopilotFlightManager(APawn * pawn, Dynamics * dynamics)
            : FFlightManager(dynamics)
        {
            _gameInput = new GameInput(pawn);

            _connected = true;
        }
		
        ~FCopilotFlightManager()
        {
        }

        virtual void getActuators(const double time, double * values) override
        {
            // Avoid null-pointer exceptions at startup, freeze after control
            // program halts
            if (!_connected) {
                return;
            }

            // Remaining values are stick demands
            _gameInput->getJoystick(&_telemetry[13]);
        }

        void tick(void)
        {
            // Get demands from keypad
            _gameInput->getKeypad(&_telemetry[13]);
        }

}; // FCopilotFlightManager
