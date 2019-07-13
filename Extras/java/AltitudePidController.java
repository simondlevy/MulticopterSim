/*
Simple altitude-hold PID controller

Copyright (C) 2019 Simon D. Levy

MIT License
*/

public class AltitudePidController {

    public AltitudePidController(double target, double posP, double velP, double velD, double windupMax)
    {
        construct(target, posP, velP, velD, windupMax);
    }

    public AltitudePidController(double target, double posP, double velP, double velD)
    {
        construct(target, posP, velP, velD, 10.0);
    }

    private void construct(double target, double posP, double velP, double velD, double windupMax)
    {
        _target = target;
        _posP = posP;
        _velP = velP;
        _velD = velD;
        _windupMax = windupMax;
    }

    private double _target;
    private double _posP;
    private double _velP;
    private double _velD;
    private double _windupMax;
}
