/*
* Debug.cpp: On-screen debugging for MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "Debug.h"
#include "OSD.h"

#include <stdarg.h>

void debug(const char * fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    char buf[200];
    vsnprintf(buf, 200, fmt, ap); 
    va_end(ap);

    osd(buf);
}

void error(const char * fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    char buf[200];
    vsnprintf(buf, 200, fmt, ap); 
    va_end(ap);

    osd(buf, true);
}


