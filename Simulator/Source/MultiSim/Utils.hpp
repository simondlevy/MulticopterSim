/*
 * UE4 utlity functions
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#define WIN32_LEAN_AND_MEAN

#include <stdio.h>
#include <stdarg.h>

#include "OSD.hpp"

// Windows/Linux compatibility 
#ifdef _WIN32
#define SPRINTF sprintf_s
#else
#include <wchar.h>
#define SPRINTF sprintf
#endif

static const FName makeName(const char * prefix, const uint8_t index, const char * suffix="")
{
    char name[200];
    SPRINTF(name, "%s%d%s", prefix, index+1, suffix);
    return FName(name);
}

static const FName makeMeshName(const char * prefix, const uint8_t index)
{
    return makeName(prefix, index, "Mesh");
}

static const FName makeMeshName(const char * prefix)
{
    return makeName(prefix, 0);
}

static void debug(const char * fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	char buf[200];
	vsnprintf(buf, 200, fmt, ap);
	osd(buf, false);
	va_end(ap);
}

static void debugline(const char * fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	char buf[200];
	vsnprintf(buf, 200, fmt, ap);
	osd(buf, false, true);
	va_end(ap);
}

static void mysprintf(char buf[200], const char * fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	vsnprintf(buf, 200, fmt, ap);
	osd(buf, false, true);
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
