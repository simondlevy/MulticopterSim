/*
Simple altitude-hold PID controller

Copyright (C) 2019 Simon D. Levy

MIT License
 */

public class AltitudePidController {

    public AltitudePidController(double target, double posP, double velP, double velI, double velD, double windupMax)
    {
        construct(target, posP, velP, velI, velD, windupMax);
    }

    public AltitudePidController(double target, double posP, double velP, double velI, double velD)
    {
        construct(target, posP, velP, velI, velD, 10.0);
    }

    public double u(double alt, double vel, double dt)
    {
        // Compute dzdt setpoint and error
        double velTarget = (_target - alt) * _posP;
        double velError = velTarget - vel;

        // Update error integral and error derivative
        _integralError +=  velError * dt;
        _integralError = AltitudePidController.constrainAbs(_integralError + velError * dt, _windupMax);
        double deltaError = java.lang.Math.abs(_lastError) > 0 ? (velError - _lastError) / dt : 0;
        _lastError = velError;

        // Compute control u
        return _velP * velError + _velD * deltaError + _velI * _integralError;
    }

    private double _target;
    private double _posP;
    private double _velP;
    private double _velI;
    private double _velD;
    private double _windupMax;
    private double _posTarget;
    private double _lastError;
    private double _integralError;

    private void construct(double target, double posP, double velP, double velI, double velD, double windupMax)
    {
        // In a real PID controller, this would be a set-point
        _target = target;

        // Constants
        _posP = posP;
        _velP = velP;
        _velI = velI;
        _velD = velD;
        _windupMax = windupMax;

        // Values modified in-flight
        _posTarget      = 0;
        _lastError      = 0;
        _integralError  = 0;
    }

    private static double constrainAbs(double x, double lim)
    {
        return x < -lim ? -lim : (x > +lim ? +lim : x);
    }
}
