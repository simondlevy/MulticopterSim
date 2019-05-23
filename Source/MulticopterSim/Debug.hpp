/*
* On-screen debugging for MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "OSD.h"

#include <stdarg.h>
#include <stdio.h>

static void debug(const char * fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	char buf[200];
	vsnprintf(buf, 200, fmt, ap);
	osd(buf, false);
	va_end(ap);
}

static void error(const char * fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	char buf[200];
	vsnprintf(buf, 200, fmt, ap);
	osd(buf, true);
	va_end(ap);
}
