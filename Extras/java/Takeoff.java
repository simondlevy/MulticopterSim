/*
   Java Multicopter test 

   Copyright(C) 2019 Simon D.Levy

   MIT License
 */

public class Takeoff {

    // Target params
    static final double ALTITUDE_START  = 0;
    static final double ALTITUDE_TARGET = 10;
    static final double VARIO_TOLERANCE = 1.e-5; //g level-off velocity

    // PID params
    static final double ALT_P = 1.25;
    static final double VEL_P = 1.5;
    static final double VEL_I = 1.0;
    static final double VEL_D = 0.05;

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
        Multicopter copter = new Multicopter("127.0.0.1", 5000, 5001);

        // Start the simulation
        copter.start();

        // Loop till level-off
        while (true) {

            // Get vehicle state from sim
            double [] telem = copter.getState();

            // Extract time, altitude from state.  Altitude is in NED coordinates, so we negate it to use as input
            // to PID controller.
            double t =  telem[0];
            z = -telem[9];

            System.out.printf("%3.3f,%+3.3f\n", t, z);

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

            // If altitude has leveled off, halt
            if (Math.abs(z) != 0 && Math.abs(dzdt) < VARIO_TOLERANCE) {
                break;
            }
        }

        // Stop simulation
        copter.stop();
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
