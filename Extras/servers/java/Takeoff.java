/*
   Java Multicopter test 

   Copyright(C) 2019 Simon D.Levy

   MIT License
 */

public class Takeoff {

    // Target params
    static final double ALTITUDE_TARGET = 10;

    // PID params
    static final double ALT_P = 1.0;
    static final double VEL_P = 1.0;
    static final double VEL_I = 0;
    static final double VEL_D = 0;

    public static void main(String [] args)
    {
        // initial conditions
        double z = 0;
        double zprev = 0;
        double tprev = 0;
        double dzdt = 0;
        double u = 0;

        // Create PID controller
        AltitudePidController pid  = new AltitudePidController(ALTITUDE_TARGET, ALT_P, VEL_P, VEL_I, VEL_D);

        // Create a multicopter simulation
        Multicopter copter = new Multicopter();

        // Start the simulation
        copter.start();

        // Loop until user hits the stop button
        while (true) {

            // Get vehicle state from sim
            double [] telem = copter.getState();

            // Extract time from state.  
            double t =  telem[0];

            // Negative time means quit
            if (t < 0) break;

            //Extract altitude from state.  Altitude is in NED coordinates, so we negate it to use as input to PID controller.
            z = -telem[9];

            // Compute vertical climb rate as first difference of altitude over time
            if (t > tprev) {

                // Use temporal first difference to compute vertical velocity
                double dt = t - tprev;
                dzdt = (z-zprev) / dt;

                // Get correction from PID controller
                u = pid.u(z, dzdt, dt);

                // Constrain correction to [0,1] to represent motor value
                u = Math.max(0., Math.min(1., u));
            }

            // Set motor values in sim
            copter.setMotors(ones(u, 4));

            // Update for first difference
            zprev = z;
            tprev = t;

            // Yield to Multicopter thread
            try {
                Thread.sleep(1);
            }
            catch (Exception e) {
            }

        }
    }

    // Helper function like Matlab/Numpy ones()
    static double [] ones(double x, int n)
    {
        double []  v = new double [n];
        for (int i=0; i<n; ++i) {
            v[i] = x;
        }
        return v;
    }
}
