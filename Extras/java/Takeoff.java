/*
   Java Multicopter test 

   Copyright(C) 2019 Simon D.Levy

   MIT License
 */

public class Takeoff {

    // Target params
    static final double ALTITUDE_START  = 0;
    static final double ALTITUDE_TARGET = 10;
    static final double VARIO_TOLERANCE = .01; //g level-off velocity

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

        double [] motorVals = new double[4];

        while (true) {
            break;
        }

        copter.stop();
    }
}
