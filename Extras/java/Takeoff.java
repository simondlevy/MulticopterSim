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

        Multicopter copter = new Multicopter("127.0.0.1", 5000, 5001);

        copter.start();

        double [] motorVals = new double[4];

        try {
            while (true) {

                Thread.sleep(1000);

                copter.setMotors(motorVals);

                double [] state = copter.getState();

                System.out.printf("t: %6.3f | g: %+3.3f %+3.3f %+3.3f | q: %+3.3f %+3.3f %+3.3f %+3.3f | p: %+3.3f %+3.3f %+3.3f\n", 
                        state[0],
                        state[1], state[2], state[3],
                        state[4], state[5], state[6], state[7],
                        state[8], state[9], state[10]);

                for (int j=0; j<motorVals.length; ++j) {
                    motorVals[j] += 0.1;
                }
            }
        }
        catch (Exception e) {
        }

        copter.stop();
    }
}
