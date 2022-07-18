#define WIN32_LEAN_AND_MEAN

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"


class FRustFlightManager : public FFlightManager {

    private:

    protected:

        virtual void getMotors(double time, double* values) override;

    public:

        FRustFlightManager(APawn * pawn, Dynamics * dynamics);

        ~FRustFlightManager();

        void tick(void);

}; // FRustFlightManager
