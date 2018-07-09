'''
Python-based loiter for HackflightSim

Copyright 2018 Simon D. Levy

MIT License
'''

class PythonLoiter(object):

    def __init__(self, varioP, varioI):

        self.varioP = varioP
        self.varioI = varioI

        self.varioIntegral = 0
        self.inBandPrev = False

        self.STICK_DEADBAND = 0.15
        self.THROTTLE_SCALE = 0.1
    
    def modifyThrottle(self, throttle, variometer):

        # Reset integral if moved into stick deadband
        inBandCurr = self._inBand(throttle)
        if inBandCurr and not self.inBandPrev:
            self.varioIntegral = 0
        self.inBandPrev = inBandCurr

        # Inside stick deadband, adjust by variometer; outside deadband, respond weakly to stick demand
        throttle = -self.varioP * variometer - self.varioI * self.varioIntegral  if inBandCurr else self.THROTTLE_SCALE * throttle;

        # Accumulate integrals
        self.varioIntegral += variometer

        return throttle

  
    def _inBand(self, demand):

        return abs(demand) < self.STICK_DEADBAND

