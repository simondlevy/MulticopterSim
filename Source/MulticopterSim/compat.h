/*
 * Windows/Linux compatibility 
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#ifdef _WIN32
#define SPRINTF sprintf_s
#else
#define SPRINTF sprintf
#endif
