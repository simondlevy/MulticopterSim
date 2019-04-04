/*
 * Accelerometer.h: Platform-indpendent class for emulating IMU accelerometer 
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

class Accelerometer {

    private:

        double _g;

    public:

        Accelerometer(double g);

        void computeImuAcceleration(
                double verticalAcceleration,
                double rotation[3], 
                double imuLinearAccelerationXYZ[3]);
};
