/*
 * Accelerometer.h: Platform-indpendent class for emulating IMU accelerometer 
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

class Accelerometer {

    public:

        static void computeImuAcceleration(
                double verticalAcceleration,
                double rotation[3], 
                double imuLinearAccelerationXYZ[3],
                double G = 9.80665);
};
