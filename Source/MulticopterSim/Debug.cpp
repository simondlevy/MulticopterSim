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

static void message(bool err, const char * fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    char buf[200];
    vsnprintf(buf, 200, fmt, ap); 

    osd(buf, err);
}

void debug(const char * fmt, ...)
{
    message(false, fmt);
}

void error(const char * fmt, ...)
{
    message(true, fmt);
}


