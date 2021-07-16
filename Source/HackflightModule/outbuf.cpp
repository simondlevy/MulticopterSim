/*
* Implemenation of rft::Debugger::outbuf() for sim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include <RFT_pure.hpp>

#include "../MainModule/Utils.hpp"

void rft::Debugger::outbuf(char * msg)
{
    debugline(msg);
}
