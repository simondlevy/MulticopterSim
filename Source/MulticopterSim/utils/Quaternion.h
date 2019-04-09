/*
 * Quaternion.h: Platform-indpendent class for quaternion compuation
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

class Quaternion {

    private:

        double _q[4];

    public:

        Quaternion(void);

        double * computeQuaternion(double eulerAngles[3]);
};
