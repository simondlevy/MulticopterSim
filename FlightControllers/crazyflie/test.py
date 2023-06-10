#!/usr/bin/env python3
'''
Simple take-off-and-move-forward script

Copyright (C) 2021 Simon D. Levy

MIT License
'''

from crazyflie_client import CrazyflieClient


def main():

    client = CrazyflieClient()

    client.start()


main()
