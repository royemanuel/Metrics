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
# an aircraft part needs to work, fail, and be part of an aircraft
class Part(object):
    def __init__(self, env, ID):
        self.ID = ID
        self.age = 0
        self.fail = 0
        self.status = "UP"


# The parts have different types of parts. I think this is the way
# to nest or do the inheritance part of OOP.
# Airframes are the source of the BuNo
class Airframe(Part):
    def __init__(self, env, ID):
        self.env = env
        super().__init__(env, ID)
        self.status = 1


# Avionics and Propusion behave the same. They will just draw from
# different distributions and be party to different updates / Tech
# refreshes.


class Avionics(Part):
    def __init__(self, env, ID):
        self.env = env
        self.status = 1
        super().__init__(env, ID)


class Propulsion(Part):
    def __init__(self, env, ID):
        self.env = env
        self.status = 1
        super().__init__(env, ID)


class Aircraft(object):
    def __init__(self, env, af, av, puls):
        self.env = env
        self.af = af
        self.av = av
        self.puls = puls
        self.BuNo = self.af.ID

    #def start(self, env):
        # if (self.af.age() >)
    # def fly(self, env):
        


env = simpy.Environment()


# Create an airplane to 
av1 = Avionics(env, "av1")
af1 = Airframe(env, "af1")
puls1 = Propulsion(env, "puls1")
ac1 = Aircraft(env, af1, av1, puls1)
