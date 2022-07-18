#define WIN32_LEAN_AND_MEAN

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"


class FRustFlightManager : public FFlightManager {

    private:

        typedef float f32;

        struct Vec2 {
            f32 x;
            f32 y;
        };

        typedef Vec2 (*vec2_init_t)();

        vec2_init_t _vec2_init;

    protected:

        virtual void getMotors(double time, double* values) override;

    public:

        FRustFlightManager(APawn * pawn, Dynamics * dynamics);

        ~FRustFlightManager();

        void tick(void);

}; // FRustFlightManager
