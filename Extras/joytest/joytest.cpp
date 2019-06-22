/*
 * Test code for joystick on Linux
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include <Joystick.h>
#include <stdio.h>

int main(int argc, char ** argv)
{
    Joystick js;

    printf("%d\n", js.isRcTransmitter());

    return 0;
}


