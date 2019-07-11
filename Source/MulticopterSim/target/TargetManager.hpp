/*
 * Abstract class for moving around the target pawn in MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "Runtime/Core/Public/Math/Vector.h"
#include "../ThreadedWorker.hpp"

class  FTargetManager : public FThreadedWorker {

    protected:

        FVector _location;

        // Constructor, called on main thread
        FTargetManager(void) : FThreadedWorker()
        {
        }

        // Called on separate thread
        virtual void performTask(double currentTime)  override
        {
            computeLocation(currentTime);
        }

        virtual void computeLocation(double currentTime) = 0;

    public:

        // Called on main thread
        const FVector & getLocation(void)
        {
            return _location;
        }

        virtual ~FTargetManager(void)
        {
        }

        // Factory method implemented by your subclass
        static FTargetManager * create(void);
};
