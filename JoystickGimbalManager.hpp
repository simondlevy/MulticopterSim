/*
   MulticopterSim GimbalManager class implementation using joystick or game controller

   Just spins propellers

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include "../MainModule/GimbalManager.hpp"

class FJoystickGimbalManager : public FGimbalManager {

    public:

        // Constructor
        FJoystickGimbalManager() : FGimbalManager() 
        {
        }

        virtual ~FJoystickGimbalManager(void)
        {
        }

        virtual void set(double currentTime, float & roll, float & pitch, float & yaw, float & fov) override
        {
            extern void getGimbalFromFlightManager(float & roll, float & pitch, float & yaw, float & fov) ;

            getGimbalFromFlightManager(roll, pitch, yaw, fov) ;

        }

}; // StickGimbalManager
