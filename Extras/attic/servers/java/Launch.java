/*
   Java Multicopter test 

   Copyright(C) 2019 Simon D.Levy

   MIT License
 */

public class Launch {

    // Target params
    static final double ALTITUDE_TARGET = 10;

    // PID params
    static final double ALT_P = 1.0;
    static final double VEL_P = 1.0;
    static final double VEL_I = 0;

    public static void main(String [] args)
    {
        // Create PID controller
        LaunchPidController pid  = new LaunchPidController(ALTITUDE_TARGET, ALT_P, VEL_P, VEL_I);

        // Create mixer
        Mixer mixer = new Mixer();

        // Create a multicopter simulation
        Multicopter copter = new Multicopter();

        // Start the simulation
        copter.start();

        // Loop until user hits the stop button
        while (true) {

            // Get time from sim
            double t = copter.getTime();

            // Negative time means quit
            if (t < 0) break;

            // Get state from sim
            double [] state = copter.getVehicleState();

            // Extract altitude and its first derivative from state.  Launch
            // is in NED coordinates, so we negate it to use as input to PID
            // controller.
            double z = -state[Multicopter.STATE_Z];
            double dz = -state[Multicopter.STATE_DZ];

            // Start with zeros for demands
            double []  omega = new double[4];


            // Get demands from PID controller
            double [] u = pid.getDemands(z, dz, t);

            // Run demands through mixer to get motor spins
            omega = mixer.getMotors(u);

            // Constrain correction to [0,1] to represent motor value
            for (int k=0; k<4; ++k) {
                omega[k] = Math.max(0., Math.min(1., omega[k]));
            }

            // Set motor values in sim
            copter.setMotors(omega);

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
