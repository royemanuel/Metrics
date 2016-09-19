# Simulate the behavior of a fleet of aircraft. Begin by
# importing what we need.

import simpy
import numpy as np
import scipy.stats as st
import pandas as pd
import pdb
# pdb.set_trace()
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
        self.fltHours = 0
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
        self.ageFail = 100
        self.fltFail = np.random.random_integers(1,25)
        print("Aircraft " + str(self.ID) + " " + str(self.fltFail))


# Avionics and Propusion behave the same. They will just draw from
# different distributions and be party to different updates / Tech
# refreshes.
class Avionics(Part):
    def __init__(self, env, ID):
        self.env = env
        super().__init__(env, ID)
        self.ageFail = 10
        self.fltFail = np.random.random_integers(1,25)
        print("Aircraft " + str(self.ID) + " " + str(self.fltFail))


class Propulsion(Part):
    def __init__(self, env, ID):
        self.env = env
        super().__init__(env, ID)
        self.ageFail = 10
        self.fltFail = np.random.random_integers(1,25)
        print("Aircraft " + str(self.ID) + " " + str(self.fltFail))


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
        if self.status:
            print("Airplane still worky")
        else:
            print("Plane broke dick")
        stud.hours = stud.hours + fltTime  # the way this is written is
        inst.hours = inst.hours + fltTime  # problematic for inst syllabus
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
        super().__init__(env, ID, exp)


class AirFramer(Maintainer):
    def __init__(self, env, ID, exp):
        super().__init__(env, ID, exp)


class AvMech(Maintainer):
    def __init__(self, env, ID, exp):
        super().__init__(env, ID, exp)


######################################################################
# Processes Defined                                                  #
######################################################################

def flight(env, ac, stud, inst):
    if ac.status:
        ft = np.random.random([1]) + 0.5
        ac.flyAircraft(env, ft, stud, inst)
        print("Tempted death again in aircraft " + str(ac.BuNo) + "!")
        yield env.timeout(ft)
    else:
        print("Side number " + str(ac.BuNo) + " is broke, fool!")
    # Hard code three hours to the next event 
    yield env.timeout(3)



class Scheduler(object):
    def __init__(self, env, acList, studList, instList):
        self.env = env
        self.acList = acList
        self.studList = studList
        self.instList = instList
        self.action = env.process(self.dailyFlightSked())

# Goals for this. Pick out a student. Assign an instructor from top of
# the instructor list. Pick an aircraft at random from the aircraft
# list. The aircraft list should contain aircraft where status is True
    def dailyFlightSked(self):
        while True:
            for stud in range(len(self.studList)):
                fltStud = self.studList.pop(0)
                fltInst = self.instList.pop(0)
                ac = self.acList[np.random.random_integers(0, len(self.acList) - 1)]
                yield self.env.process(flight(self.env, ac, fltStud, fltInst))
                self.studList.extend([fltStud])
                self.instList.extend([fltInst])


# def repair(env, ac)
env = simpy.Environment()


# Create an airplane to
av1 = Avionics(env, "av1")
af1 = Airframe(env, "af1")
puls1 = Propulsion(env, "puls1")
ac1 = Aircraft(env, af1, av1, puls1)
av2 = Avionics(env, "av2")
af2 = Airframe(env, "af2")
puls2 = Propulsion(env, "puls2")
ac2 = Aircraft(env, af2, av2, puls2)

availAC = [ac1, ac2]

stud1 = Student(env, 34)
inst1 = Instructor(env, 2, 10)


stud2 = Student(env, 34)
inst2 = Instructor(env, 2, 20)

availStud = [stud1, stud2]

availInst = [inst1, inst2]

sked = Scheduler(env, availAC, availStud, availInst)


# flight(env, ac1, stud1, inst1)
# flight(env, ac1, stud1, inst1)
# flight(env, ac1, stud1, inst1)
# flight(env, ac1, stud1, inst1)
# flight(env, ac1, stud1, inst1)
# flight(env, ac1, stud1, inst1)
# flight(env, ac1, stud1, inst1)
# flight(env, ac1, stud1, inst1)
# flight(env, ac1, stud1, inst1)
# flight(env, ac1, stud1, inst1)

env.run(until = 20)
