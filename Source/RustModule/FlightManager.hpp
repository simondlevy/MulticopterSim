#define WIN32_LEAN_AND_MEAN

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"


class FRustFlightManager : public FFlightManager {

    private:

        typedef float f32;

        struct Motors {
            f32 m1;
            f32 m2;
            f32 m3;
            f32 m4;
        };

        typedef Motors (*get_motors_t)(Dynamics::vehicle_state_t * vstate);

        get_motors_t _get_motors;

        Dynamics * _dynamics;

    protected:

        virtual void getMotors(double time, double* values) override;

    public:

        FRustFlightManager(APawn * pawn, Dynamics * dynamics);

        ~FRustFlightManager();

        void tick(void);

}; // FRustFlightManager
