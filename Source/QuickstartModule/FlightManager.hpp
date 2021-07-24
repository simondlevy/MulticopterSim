/*
   Simple MulticopterSim FlightManager class implementation

   Takes off to 10m altitude via PID control

   Copyright(C) 2021 Simon D.Levy

   MIT License
*/

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"

class FQuickstartFlightManager : public FFlightManager {

    private:

        Dynamics * _dynamics = NULL;

        class AltitudeController {

            private:

                float _Kp_z=0;
                float _Kp_dz=0;
                float _Ki_dz=0;
                float _windupMax=0;

                float _errorIntegral = 0;
                float _tprev = 0;

                static float constrainAbs(float x, float lim) {

                    return x < -lim ? -lim : (x > +lim ? +lim : x);
                }

            public: 

                AltitudeController(float Kp_z=1.0,
                                   float Kp_dz=1.0,
                                   float Ki_dz=0,
                                   float windupMax=0) {

                    _Kp_z = Kp_z;
                    _Kp_dz = Kp_dz;
                    _Ki_dz = Ki_dz;
                    _windupMax = windupMax;

                    _tprev = 0;
                    _errorIntegral = 0;
                }

                float getThrottle(float target,
                                  float t,
                                  float z,
                                  float dzdt)
                {
                    debugline("%3.3f", z);

                    // Compute dzdt setpoint and error
                    float dzdt_target = (target - z) * _Kp_z;
                    float dzdt_error = dzdt_target - dzdt;

                    // Update error integral and error derivative
                    _errorIntegral = 
                        constrainAbs(_errorIntegral + dzdt_error * (t-_tprev),
                                     _windupMax);
                        
                    // Track time
                    _tprev = t;

                    // Compute control u
                    return _Kp_dz * dzdt_error + _Ki_dz * _errorIntegral;
                }

        }; // class AltitudeController

        AltitudeController _altitudeController;

        static constexpr double ALTITUDE_TARGET = 10;

    public:

        FQuickstartFlightManager(Dynamics * dynamics)
            : FFlightManager(dynamics)
        {
            _dynamics = dynamics;
        }

        ~FQuickstartFlightManager()
        {
        }

        virtual void getActuators(const double time, double * values) override
        {
            float throttle = _altitudeController.getThrottle(
                    ALTITUDE_TARGET, 
                    time,
                    -_dynamics->x(Dynamics::STATE_Z),
                    -_dynamics->x(Dynamics::STATE_Z_DOT));

            for (uint8_t i=0; i<_dynamics->rotorCount(); ++i) {
                values[i] = throttle;
            }
        }

}; // FQuickstartFlightManager
