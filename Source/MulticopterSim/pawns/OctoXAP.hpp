/*
 Vehicle class or octo-X frames using ArduPilot motor layout:

        5CCW  1CW
          \   /
      7CW       3CCW
          \   /
            ^     
          /   \
      6CW       8CCW
          /   \
        2CCW  4CW

 For now we use quadX dynamics

 Copyright (C) 2019 Simon D. Levy, Daniel Katzav

 MIT License
*/

#pragma once

#include "Vehicle.hpp"

class OctoXAP : public Vehicle {

    private:

        const int8_t directions[4] = {+1, +1, +1, +1};

    protected:

        // MultirotorDynamics method overrides

        // roll right
        virtual double u2(double * o) override
        {
            return (o[1] + o[2]) - (o[0] + o[3]);
        }

        // pitch forward
        virtual double u3(double * o) override
        {
            return (o[1] + o[3]) - (o[0] + o[2]);
        }

        // yaw cw
        virtual double u4(double * o) override
        {
            return (o[0] + o[1]) - (o[2] + o[3]);
        }

        // motor direction for animation
        virtual int8_t motorDirection(uint8_t i) override
        {
            const int8_t dir[4] = {-1, -1, +1, +1};
            return dir[i];
        }


    public:	

        // Container for frame layout constants
        typedef struct {

            float cx;   // center X
            float cy;   // center Y
            float mo;   // motor offset
            float wd;   // width
            float ln;   // length
            float mz;   // motor Z
            float pz;   // propeller Z

        } layout_t;

        OctoXAP(const objects_t & objects, const params_t & params)
            : Vehicle(objects, params, 4) 
        {
        }

        static void addMotor(objects_t & objects, uint8_t index, int8_t dx, int8_t dy, const layout_t & l, UStaticMesh * propMesh)
        {
            float cx = l.cx + dx * l.wd;
            float cy = l.cy + dy * l.ln;
            Vehicle::addMotor(objects, index, FVector(cx, cy+l.mo, l.mz), propMesh, FVector(cx, cy, l.pz)); 
        }

        static void build(objects_t & objects, const layout_t & layout,
                UStaticMesh * prop1Mesh, UStaticMesh * prop2Mesh, UStaticMesh * prop3Mesh, UStaticMesh * prop4Mesh)
        {
            Vehicle::build(objects);

            OctoXAP::addMotor(objects, 0, +1, +1, layout, prop1Mesh);
            OctoXAP::addMotor(objects, 1, -1, -1, layout, prop2Mesh);
            OctoXAP::addMotor(objects, 2, +1, -1, layout, prop3Mesh);
            OctoXAP::addMotor(objects, 3, -1, +1, layout, prop4Mesh);
        }

}; // class OctoXAP
