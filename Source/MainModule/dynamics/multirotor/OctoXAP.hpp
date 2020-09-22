/*
* Dynamics class for octo-X frames using ArduPilot motor layout:
*
*        5CCW   1CW
*                  
*    7CW           3CCW
*                   
*             ^      
*                   
*    6CW           8CCW
*                   
*        2CCW   4CW
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "../Multirotor.hpp"

class OctoXAPDynamics : public MultirotorDynamics {

    public:	

		OctoXAPDynamics(8, double b, double d, double m, double Ix, double Iy, double Iz, double Jr, uint16_t maxrpm, double l) 
            : MultirotorDynamics(b, d, m, Ix, Iy, Iz, Jr, maxrpm, l)
        {
        }

    protected:

        // Dynamics method overrides
		
		// roll right
		virtual double u2(double * o) override
		{
			//       [2         5         6         7]     - [  1         3         4         8]
			return (C1*o[1] + C1*o[4] + C2*o[5] + C2*o[6]) - (C1*o[0] + C2*o[2] + C1*o[3] + C2*o[7]);
		}

		// pitch forward
		virtual double u3(double * o) override
		{
			//       [ 2        4         6         8]   -   [  1         3         5         7]
			return (C2*o[1] + C2*o[3] + C1*o[5] + C1*o[7]) - (C2*o[0] + C1*o[2] + C2*o[4] + C1*o[6]);
		}



        // yaw clockwise
        virtual double u4(double * o) override
        {
            //       [3      4      5      6]  -   [1      2      7      8]
            return (o[2] + o[3] + o[4] + o[5]) - (o[0] + o[1] + o[6] + o[7]);
        }

        // motor direction for animation
        virtual int8_t motorDirection(uint8_t i) override
        {
            //                      1   2   3   4   5   6   7   8                                 
            const int8_t dir[8] = {+1, +1, -1, -1, -1, -1, +1, +1};
            return dir[i];
        }

    private:

        static constexpr double C1 = 0.382680;
        static constexpr double C2 = 0.923879;

}; // class OctoXAP
