/*
   TargetController implementation

   For HackflightSim we just hide the target below the surface

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include "../MulticopterSim/TargetController.hpp"

class NullTargetController : public TargetController {

    public:

        virtual void update(float dt) override
        {
            _location.X = 0;
            _location.Y = 0;
            _location.Z = -100;
        }
}; 

SIMPLUGIN_API TargetController * createTargetController(void)
{
    return new NullTargetController();
}
