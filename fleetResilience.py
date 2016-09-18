# Simulate the behavior of a fleet of aircraft. Begin by
# importing what we need.

import simpy
import numpy as np
import scipy.stats as st
import pandas as pd
# for now we will use the Python provided module random to build random
# numbers
# import random

np.random.seed([42])


######################################################################
# Aircraft Parts and assembling them to an aircraft                  #
######################################################################


# Build an aircraft part
# an aircraft part needs to work, fail, and be part of an aircraft
class Part(object):
    def __init__(self, env, ID):
        self.ID = ID
        self.age = 0
        self.ageFail = 10
        self.fltHours = 0
        self.fltFail = 3
        self.status = True

    # Check the whether the part breaks during the operation
    def failFlight(self, env, fltTime):
        if (self.fltFail < self.fltHours + fltTime):
            self.fltHours = self.fltFail
            self.status = False
        else:
            self.fltHours = self.fltHours + fltTime

# The parts have different types of parts. I think this is the way
# to nest or do the inheritance part of OOP.
# Airframes are the source of the BuNo
class Airframe(Part):
    def __init__(self, env, ID):
        self.env = env
        super().__init__(env, ID)


# Avionics and Propusion behave the same. They will just draw from
# different distributions and be party to different updates / Tech
# refreshes.
class Avionics(Part):
    def __init__(self, env, ID):
        self.env = env
        super().__init__(env, ID)


class Propulsion(Part):
    def __init__(self, env, ID):
        self.env = env
        super().__init__(env, ID)


class Aircraft(object):
    def __init__(self, env, af, av, puls):
        self.env = env
        self.af = af
        self.av = av
        self.puls = puls
        self.BuNo = self.af.ID
        self.status = self.af.status & self.av.status & self.puls.status

    def flyAircraft(self, env, fltTime, stud, inst):
        # Check to see if any of the parts failed in flight
        self.av.failFlight(env, fltTime)
        self.af.failFlight(env, fltTime)
        self.puls.failFlight(env, fltTime)
        # Update the aircraft status
        self.status = self.af.status & self.av.status & self.puls.status
        # Update aircrew values. For now, updating flight time
        # whether up or down, and not counting a syllabus event if down
        stud.hours = stud.hours + fltTime # the way this is written is
        inst.hours = inst.hours + fltTime # problematic for inst syllabus
        if (self.status == True):
            stud.syllabus += 1


######################################################################
# People                                                             #
######################################################################


# Class of Aircrew which will be tied to an aircraft for an event.
# Each flight has an instructor and a student. A student is complete.
class Aircrew(object):
    def __init__(self, env, ID):
        self.ID = ID
        self.hours = 0
        self.dailyFlights = 0


# A student collects a number of syllabus events and graduates
# A student is limited to 2 daily flights
class Student(Aircrew):
    def __init__(self, env, ID):
        super().__init__(env, ID)
        self.syllabus = 0
        self.graduated = False


# An instructor cannot instruct until the syllabus is complete
# (qual set to True)
# An instructor is limited to 3 daily flights
class Instructor(Aircrew):
    def __init__(self, env, ID, syl):
        super().__init__(env, ID)
        self.syllabus = syl
        self.qual = self.syllabus > 9


# Defining the maintainers although this might need to be a resource.  
class Maintainer(object):
    def __init__(self, env, ID):
        self.env = env
        self.ID = ID
        self.exp = exp


class AvTech(Maintainer):
    def __init__(self, env, ID, exp):
        super()__init__(env, ID, exp)


class AirFramer(Maintainer):
    def __init__(self, env, ID, exp):
        super()__init__(env, ID, exp)


class AvMech(Maintainer):
    def __init__(self, env, ID, exp):
        super()__init__(env, ID, exp)


######################################################################
# Processes Defined                                                  #
######################################################################

def flight(env, ac, stud, inst):
    if ac.status:
        fltTime = np.random.random([1]) + 0.5
        ac.flyAircraft(env, fltTime, stud, inst)
    else:
        print("The plane's broke, fool!")

def repair(env, ac)
env = simpy.Environment()


# Create an airplane to 
av1 = Avionics(env, "av1")
af1 = Airframe(env, "af1")
puls1 = Propulsion(env, "puls1")
ac1 = Aircraft(env, af1, av1, puls1)

stud1 = Student(env, 34)
inst1 = Instructor(env, 2, 10)

flight(env, ac1, stud1, inst1)
flight(env, ac1, stud1, inst1)
flight(env, ac1, stud1, inst1)
flight(env, ac1, stud1, inst1)
flight(env, ac1, stud1, inst1)
flight(env, ac1, stud1, inst1)
flight(env, ac1, stud1, inst1)
flight(env, ac1, stud1, inst1)
flight(env, ac1, stud1, inst1)
flight(env, ac1, stud1, inst1)
