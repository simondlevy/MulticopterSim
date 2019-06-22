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

    while (true) {

        float axes[6];
        uint8_t buttons;

        if (!js.poll(axes, buttons)) {
            printf("%f\n", axes[0]);
        }
    }

    return 0;
}


