/*
   Java Mixer class for multirotors

   Takes demands U [Throttle, Roll, Pitch, Yaw] and returns motors spins Omega
   or other motor activity (e.g., coaxial servos)

   Copyright(C) 2021 Bennet Ehret, Simon D.Levy

   MIT License
 */

/**
  * This is the mixer for a quadrotor laid out in the MultiWii configuration:

    4cw   2ccw
       \ /
        ^
       / \
    3ccw  1cw

    Eventually this should be an abstract class allowing us to subclas various configurations
    (including Coaxial)

*/
public class Mixer {

    public double [] getMotors(double [] u)
    {
        double [] omega = new double[4];

        // XXX Fake up a mixer by setting all motors to throttle demand
        for (int k=0; k<4; ++k) {
            omega[k] = u[0];

        }
        return omega;
    }
}
