/*
 * Abstract class for moving around the target pawn in MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

class TargetController {

    private:

        FVector _location;

    protected:

        TargetController(float startX, float startY, float startZ) 
        {
            _location.X = startX;
            _location.Y = startY; _location.Z = startZ;
        }

    public:

        const FVector & getLocation(void)
        {
            return _location;
        }

        virtual ~TargetController(void)
        {
        }

        virtual void update(void) = 0;

        // Factory method implemented by your subclass
        static TargetController * create(void);
};
