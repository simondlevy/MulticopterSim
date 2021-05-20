/*
Simple altitude-hold PID controller

Copyright (C) 2019 Simon D. Levy

MIT License
 */

public class LaunchPidController {

    private double tprev; // previous time for computing dt

    public LaunchPidController(double target, double Kp_z, double Kp_dz, double Ki_dz, double windupMax)
    {
        construct(target, Kp_z, Kp_dz, Ki_dz, windupMax);
    }

    public LaunchPidController(double target, double Kp_z, double Kp_dz, double Ki_dz)
    {
        construct(target, Kp_z, Kp_dz, Ki_dz, 10.0);
    }

    /**
      * @param z altitude
      * @param dz altitude first temporal difference (climb rate)
      * @param t current time
      * @return demands U [throttle, roll, pitch, yaw]
      */
    public double [] getDemands(double z, double dz, double t)
    {
        // Compute dt
        double dt = t - tprev;

        // Compute dzdt setpoint and error
        double dzTarget = (_target - z) * _Kp_z;
        double dzError = dzTarget - dz;

        // Update error integral and error derivative
        _integralError +=  dzError * dt;
        _integralError = LaunchPidController.constrainAbs(_integralError + dzError * dt, _windupMax);

        // Compute control for throttle
        double [] u = new double[4];
        u[0] = _Kp_dz * dzError + _Ki_dz * _integralError;

        // If time is between five and six seconds, set pitch to a very small value (.001)
        if (5<t && t<6){
            u[2] = u[2] += .001;
        }
        // Track previous time for dt
        tprev = t;

        // Return U
        return u;
    }

    private double _target;
    private double _Kp_z;
    private double _Kp_dz;
    private double _Ki_dz;
    private double _windupMax;
    private double _posTarget;
    private double _integralError;

    private void construct(double target, double Kp_z, double Kp_dz, double Ki_dz, double windupMax)
    {
        // In a real PID controller, this would be a set-point
        _target = target;

        // Constants
        _Kp_z = Kp_z;
        _Kp_dz = Kp_dz;
        _Ki_dz = Ki_dz;
        _windupMax = windupMax;

        // Values modified in-flight
        _posTarget      = 0;
        _integralError  = 0;
    }

    private static double constrainAbs(double x, double lim)
    {
        return x < -lim ? -lim : (x > +lim ? +lim : x);
    }
}
