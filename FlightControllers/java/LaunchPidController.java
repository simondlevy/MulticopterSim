/*
   Simple altitude-hold PID controller

   Copyright (C) 2019 Simon D. Levy

   This file is part of SimFlightControl.
 
   SimFlightControl is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation, either version 3 of the License, or (at your option)
   any later version.
 
   SimFlightControl is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
   more details.
 
   You should have received a copy of the GNU General Public License along with
   SimFlightControl. If not, see <https://www.gnu.org/licenses/>.

 */

public class LaunchPidController {

    private double tprev; // previous time for computing dt

    public LaunchPidController(double target, double Kp, double Ki, double windupMax)
    {
        construct(target, Kp, Ki, windupMax);
    }

    public LaunchPidController(double target, double Kp, double Ki)
    {
        construct(target, Kp, Ki, 10.0);
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
        double dzError = (_target - z) - dz;

        // Update error integral and error derivative
        _integralError +=  dzError * dt;
        _integralError = LaunchPidController.constrainAbs(_integralError + dzError * dt, _windupMax);

        // Compute control for throttle
        double [] u = new double[4];
        u[0] = _Kp * dzError + _Ki * _integralError;

        // If time is between five and six seconds, set pitch to a very small value (.001)
        if (5<t && t<6){
            u[2] += .001;
        }
        // Track previous time for dt
        tprev = t;

        // Return U
        return u;
    }

    private double _target;
    private double _Kp;
    private double _Ki;
    private double _windupMax;
    private double _posTarget;
    private double _integralError;

    private void construct(double target, double Kp, double Ki, double windupMax)
    {
        // In a real PID controller, this would be a set-point
        _target = target;

        // Constants
        _Kp = Kp;
        _Ki = Ki;
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
