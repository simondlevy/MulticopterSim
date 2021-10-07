/*
* Implemenation of hf::Debugger::outbuf() for sim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "../MainModule/Utils.hpp"

#include <HF_debugger.hpp>

void hf::Debugger::outbuf(char * msg)
{
    debugline(msg);
}
