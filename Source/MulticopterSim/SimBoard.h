/*
* SimBoard.h: Abstract flight-controller board class for MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/


#pragma once

class MULTICOPTERSIM_API SimBoard {

public:

    SimBoard(void);

    ~SimBoard(void);

    void update(void);

    /**
     *  Factory method.
     *  @return pointer to a new SimBoard object
     */
     static SimBoard * createSimBoard(void);
};
