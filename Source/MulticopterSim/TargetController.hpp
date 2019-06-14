/*
 * Abstract class for moving around the target pawn in MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "Runtime/Core/Public/Math/Vector.h"

class  TargetController {

    protected:

        FVector _location;

        TargetController(void)
        {
        }

    public:

        const FVector & getLocation(void)
        {
            return _location;
        }

        virtual ~TargetController(void)
        {
        }

        virtual void update(float dt) = 0;

        // Factory method implemented by your subclass
        static TargetController * create(void);
};
