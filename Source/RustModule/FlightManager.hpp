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

        struct VehicleState {
            f32 x;
            f32 dx;
            f32 y;
            f32 dy;
            f32 z;
            f32 dz;
            f32 phi;
            f32 dphi;
            f32 theta;
            f32 dtheta;
            f32 psi;
            f32 dpsi;
        };

        typedef Motors (*get_motors_t)(VehicleState * vstate);

        get_motors_t _get_motors;

    protected:

        virtual void getMotors(double time, double* values) override;

    public:

        FRustFlightManager(APawn * pawn, Dynamics * dynamics);

        ~FRustFlightManager();

        void tick(void);

}; // FRustFlightManager
