/*
   MulticopterSim GimbalManager class implementation using a stub

   Just spins propellers

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/GimbalManager.hpp"

class FStickGimbalManager : public FGimbalManager {

    public:

        // Constructor
        FStickGimbalManager() : FGimbalManager() 
        {
        }

        virtual ~FStickGimbalManager(void)
        {
        }

        virtual void set(double currentTime, float & roll, float & pitch, float & yaw, float & fov) override
        {
            extern void getGimbalFromFlightManager(float & roll, float & pitch, float & yaw, float & fov) ;

            getGimbalFromFlightManager(roll, pitch, yaw, fov) ;

        }

}; // StickGimbalManager


// Factory method for GimbalManager class
FLIGHTMODULE_API FGimbalManager * createGimbalManager(void)
{
    return new FStickGimbalManager();
}
