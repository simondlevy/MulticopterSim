#pragma once

#define WIN32_LEAN_AND_MEAN

#include <stdio.h>
#include <stdarg.h>

#include <debug.h>

#include "../../MainModule/OSD.hpp"

// Windows/Linux compatibility 
#ifdef _WIN32
#define SPRINTF sprintf_s
#else
#include <wchar.h>
#define SPRINTF sprintf
#endif

extern "C" {

void debugPrintf(const char * fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	char buf[200];
	vsnprintf(buf, 200, fmt, ap);
	osd(buf, false, true);
	va_end(ap);
}

} // extern "C"
