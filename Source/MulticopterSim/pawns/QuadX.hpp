/*
* Vehicle class or quad-X frames:
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "Vehicle.hpp"

class QuadX : public Vehicle {

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

        QuadX(const objects_t & objects, MultirotorDynamics * dynamics) : Vehicle(objects, dynamics) 
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

}; // class QuadX
