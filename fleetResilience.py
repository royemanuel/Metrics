# Simulate the behavior of a fleet of aircraft. Begin by
# importing what we need.

import simpy
import numpy as np
import scipy.stats as st
import pandas as pd
# for now we will use the Python provided module random to build random
# numbers
import random

RS = random.seed(42)


# Build an aircraft part
class Part(object):
    def __init__(self): class Airframe(Part):
    def __init__(self, env, BuNo):
        self.env = env
        self.BuNo = BuNo
        self.status = 1


class Avionics(Part):
    def __init__(self, env):
        self.env = env
        self.status = 1


class Propulsion(Part):
    def __init__(self, env):
        self.env = env
        self.status = 1


class Aircraft(object):
    def __init__(self, env, af, av, puls):
        self.env = env
        self.af = af
        self.av = av
        self.puls = puls
        self.BuNo = af.BuNo
        self.fly_proc = env.process(self.fly(env))

    def fly(self, env):
        
