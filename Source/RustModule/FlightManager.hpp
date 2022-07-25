#define WIN32_LEAN_AND_MEAN

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"


class FRustFlightManager : public FFlightManager {

    private:

        typedef struct {

            float throttle;
            float roll;
            float pitch;
            float yaw;

        } demands_t;

        typedef struct {

            float m1;
            float m2;
            float m3;
            float m4;

        } motors_t;

        typedef motors_t (*get_motors_t)(
                demands_t * demands, Dynamics::vehicle_state_t * vstate);

        get_motors_t _get_motors;

        Dynamics * _dynamics;

    protected:

        virtual void getMotors(double time, double* values) override;

    public:

        FRustFlightManager(APawn * pawn, Dynamics * dynamics);

        ~FRustFlightManager();

        void tick(void);

}; // FRustFlightManager
