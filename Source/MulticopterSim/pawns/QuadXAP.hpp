/*
* Vehicle class or quad-X frames using ArduPilot motor layout:
*
*    3cw   1ccw
*       \ /
*        ^
*       / \
*    2ccw  4cw
*
* Copyright (C) 2019 Simon D. Levy, Daniel Katzav
*
* MIT License
*/

#pragma once

#include "Vehicle.hpp"
#include "dynamics/QuadXAP.hpp"

class QuadXAP : public Vehicle {

    public:	

        // Container for frame layout constants
        typedef struct {

            float wd;   // width
            float ln;   // length
            float cx;   // center X
            float cy;   // center Y
            float pz;   // propeller Z
            float mo;   // motor offset
            float mz;   // motor Z

        } layout_t;

        QuadXAP(const objects_t & objects, const params_t & params)
            : Vehicle(objects, new QuadXAPDynamics(params), params, 4) 
        {
        }

        static void build(objects_t & objects, const layout_t & layout,
                UStaticMesh * prop1Mesh, UStaticMesh * prop2Mesh, UStaticMesh * prop3Mesh, UStaticMesh * prop4Mesh)
        {
            Vehicle::build(objects);

            addMotorAndProp(objects, 0, +1, +1, layout, prop1Mesh);
            addMotorAndProp(objects, 1, -1, -1, layout, prop2Mesh);
            addMotorAndProp(objects, 2, +1, -1, layout, prop3Mesh);
            addMotorAndProp(objects, 3, -1, +1, layout, prop4Mesh);
        }

    protected:

        // Methods for adding mesh components using frame layout constraints

        static void addProp(objects_t & objects, uint8_t index, float dx, float dy, const layout_t & l, UStaticMesh * propMesh)
        {
            float cx = l.cx + dx * l.wd;
            float cy = l.cy + dy * l.ln;

            Vehicle::addProp(objects, index, cx, cy, l.pz, propMesh);
        }

        static void addMotorAndProp(objects_t & objects, uint8_t index, float dx, float dy, const layout_t & l, UStaticMesh * propMesh)
        {
            float cx = l.cx + dx * l.wd;
            float cy = l.cy + dy * l.ln;

            Vehicle::addMotorAndProp(objects, index, cx, cy, l.pz, l.mo, l.mz, propMesh);
        }

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

}; // class QuadXAP
