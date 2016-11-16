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

######################################################################
# Build the historical DataFrames                                    #
######################################################################

######################################################################
# Aircraft Parts and assembling them to an aircraft                  #
######################################################################


# Build an aircraft part
# an aircraft part needs to work, fail, and be part of an aircraft
class Part(object):
    def __init__(self, env, ID):
        self.ID = ID
        self.bornDate = env.now
        self.age = 0
        self.fltHours = 0
        self.fltHrsSinceFail = 0
        self.status = True
        self.history = pd.DataFrame()
        # hardcoding this in to demo failtTime
        self.endtime = 25
        self.fltFail = self.failTime(env, **{"endTime": self.endtime})

    # Define a fail time for the particular part.
    def failTime(self, env, **kwargs):
        for kw in kwargs:
            if kwargs[kw] == "endTime":
                self.fltFail = np.random.random_integers(1, kwargs[kw]) 

    # Check the whether the part breaks during the operation
    def failFlight(self, env, fltTime):
        if (self.fltFail < self.fltHrsSinceFail + fltTime):
            self.fltHours = self.fltHours + self.fltFail - self.fltHrsSinceFail
            self.status = False
            self.age = env.now - self.bornDate
            self.history = self.history.append({"ID": self.ID,
                                               "Age": self.age,
                                               "FlightHours": self.fltHours},
                                               ignore_index=True)
            # THis section will go with a repair function, but for now
            # I want to check the validity
            self.failTime(env, **{"endTime": self.endtime})
            self.fltHrsSinceFail = 0
        else:
            self.fltHrsSinceFail += fltTime
            self.fltHours += fltTime
            self.age = env.now - self.bornDate
            self.history = self.history.append({"ID": self.ID,
                                                "Age": self.age,
                                                "FlightHours": self.fltHours,
                                                "fhSinceFail": self.fltHrsSinceFail},
                                                ignore_index=True)



# The parts have different types of parts. I think this is the way
# to nest or do the inheritance part of OOP.
# Airframes are the source of the BuNo
class Airframe(Part):
    def __init__(self, env, ID):
        self.env = env
        self.obj = "Airframe"
        super().__init__(env, ID)
        self.ageFail = 100
        # This needs to be a call to a method for part. ID the parameters
        # in the particular part
        self.fltFail = np.random.random_integers(1, 25) 
        print("Aircraft " + str(self.ID) + " " + str(self.fltFail))


# Avionics and Propusion behave the same. They will just draw from
# different distributions and be party to different updates / Tech
# refreshes.
class Avionics(Part):
    def __init__(self, env, ID):
        self.env = env
        self.obj = "Avionics"
        super().__init__(env, ID)
        self.ageFail = 10
        self.fltFail = np.random.random_integers(1, 25) 
        print("Aircraft " + str(self.ID) + " " + str(self.fltFail))


class Propulsion(Part):
    def __init__(self, env, ID):
        self.env = env
        self.obj = "Propulsion"
        super().__init__(env, ID)
        self.ageFail = 10
        self.fltFail = np.random.random_integers(1, 25) 
        print("Aircraft " + str(self.ID) + " " + str(self.fltFail))


class Aircraft(object):
    def __init__(self, env, af, av, puls):
        self.env = env
        self.obj = "Aircraft"
        self.af = Airframe(env, af)
        self.av = Avionics(env, av)
        self.puls = Propulsion(env, puls)
        self.BuNo = self.af.ID
        self.status = self.af.status & self.av.status & self.puls.status
        self.bornDate = env.now
        self.blueBook = pd.DataFrame()

    def updateBlueBook(self, env, fltTime):
        print("Updating Blue Book")
        self.blueBook = self.blueBook.append({"Aircraft": self.BuNo,
                                              "FlightHours": fltTime,
                                              "AC Status": self.status,
                                              "Airframe Status": self.af.status,
                                              "Avionics Status": self.av.status,
                                              "Propulsion Status": self.puls.status,
                                              "Flight Date": env.now},
                                             ignore_index=True)
        if self.status is False:
            if self.af.status is False:
                repTime = 15
                yield env.timeout(repTime)
                self.af.status = True
                self.av.status = True
                self.puls.status = True
                print("The Airframe was broken, but now we fixed everything")
            elif self.puls.status is False:
                repTime = 15
                yield env.timeout(repTime)
                self.puls.status = True
                self.av.status = True
                print("The Engine busted, so we fixed that and checked the instruments")
            elif self.av.status is False:
                repTime = 15
                yield env.timeout(repTime)
                self.av.status = True
                print("Just the instruments were down. Up and at'em")
            self.status = self.af.status & self.av.status & self.puls.status
            self.blueBook = self.blueBook.append({"Aircraft": self.BuNo,
                                                  "FlightHours": fltTime,
                                                  "AC Status": self.status,
                                                  "Airframe Status": self.af.status,
                                                  "Avionics Status": self.av.status,
                                                  "Propulsion Status": self.puls.status,
                                                  "RepairTime": repTime},
                                                 ignore_index=True)

    def flyAircraft(self, env, fltTime, stud, inst):
        # Check to see if any of the parts failed in flight
        self.av.failFlight(env, fltTime)
        # histUpdate(self.av)
        self.af.failFlight(env, fltTime)
        self.puls.failFlight(env, fltTime)
        # Update the aircraft status
        self.status = self.af.status & self.av.status & self.puls.status
        # self.blueBook = self.blueBook.append({"Aircraft": self.BuNo,
        #                                       "FlightHours": fltTime,
        #                                       "AC Status": self.status,
        #                                       "Airframe Status": self.af.status,
        #                                       "Avionics Status": self.av.status,
        #                                       "Propulsion Status": self.puls.status,
        #                                       "Flight Date": env.now},
        #                                      ignore_index=True)
        # self.updateBlueBook(env, fltTime)
        # Update aircrew values. For now, updating flight time
        # whether up or down, and not counting a syllabus event if down
        if self.status:
            print("Airplane " + str(self.BuNo) + " still worky")
        else:
            print("Plane broke dick")
        stud.hours = stud.hours + fltTime
        inst.hours = inst.hours + fltTime
        stud.flightLog(env, fltTime, self.BuNo)
        inst.flightLog(env, fltTime, self.BuNo)
        env.process(self.updateBlueBook(env, fltTime))
        if self.status is True:
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
        self.flightDF = pd.DataFrame()


    def flightLog(self, env, fltTime, ac):
        print("Updating Flight Log")
        self.flightDF = self.flightDF.append({"Stud ID": self.ID,
                                             "Flight Time": fltTime,
                                             "Aircraft": ac,
                                             "Takeoff Time": env.now},
                                             ignore_index=True)


# A student collects a number ofsyllabus events and graduates
# A student is limited to 2 daily flights
class Student(Aircrew):
    def __init__(self, env, ID):
        self.obj = "Student"
        super().__init__(env, ID)
        self.syllabus = 0
        self.graduated = False


# An instructor cannot instruct until the syllabus is complete
# (qual set to True)
# An instructor is limited to 3 daily flights
class Instructor(Aircrew):
    def __init__(self, env, ID, syl):
        self.obj = "Instructor"
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
        ft = np.random.random([1])  + 0.5
        ac.flyAircraft(env, ft, stud, inst)
        print(inst.ID, "and", stud.ID, "tempted death again in aircraft",
              ac.BuNo, "at time", env.now, "for", ft, "hours!")
        yield env.timeout(ft)
    else:
        print("Side number " + str(ac.BuNo) + " is broke, fool!")
    # Hard code three hours to the next event 
    yield env.timeout(3)


class Scheduler(object):
    def __init__(self, env, fl, studList, instList):
        self.env = env
        self.studList = studList
        self.flightLine = fl
        self.instList = instList
        self.action = env.process(self.dailyFlightSked())

# Goals for this. Pick out a student. Assign an instructor from top of
# the instructor list. Pick an aircraft at random from the aircraft
# list. The aircraft list should contain aircraft where status is True
    def dailyFlightSked(self):
        while True:
            # numStuds = len(self.studList)
            # numAC = len(self.flightLine)
            # num = numStuds if numStuds <= numAC else numAC
            for flt in range(len(self.studList)):
                fltStud = self.studList[flt]
                # fltStud = self.studList.pop(0)
                # print("Yup "+ str(fltStud.ID))
                fltInst = self.instList.pop(0)
                print("getting ac")
                ac = self.flightLine.pop(0)
                print("got ac" + ac.BuNo)
                # # self.acList[np.random.random_integers(0, len(self.acList) - 1)]
                # print("Stud Vars ")
                # print(vars(fltStud))
                # print("Inst Vars ")
                # print(vars(fltInst))
                yield self.env.process(flight(self.env, ac, fltStud, fltInst))
                self.flightLine.append(ac)
                # self.studList.extend([fltStud])
                # print(str(self.studList[0].ID) + str(self.studList[1].ID))
                self.instList.extend([fltInst])
                # print(str(self.instList[0].ID) + str(self.instList[1].ID))


# Build an aircraft. If it is the start, it will build 
def buildAC(env, numAC, fl):
    for n in range(numAC):
        af = "af" + str(n)
        av = "av" + str(n)
        eng = "eng" + str(n)
        ac = Aircraft(env, af, av, eng)
        fl.append(ac)


######################################################################
# Useful Functions and Classes                                       #
######################################################################

# This is a catch all function for updating the dataframe storing the
# history of whatever it is, airframe, aircraft, etc. I'm keeping it all
# here so I know where to change it if I need to.
## ***** Right now I just store the vars, but I know that is wrong for
## Stud, inst, and AC ***************
# I think I'm going to build the methods into the classes. So delete this
# if you do that
# def histUpdate(item):
#     if item.obj == "Airframe" or item.obj == "Avionics" or item.obj == "Propulsion":
#         partHistory = partHistory.append(vars(item))
#     elif item.obj == "Student":
#         studHistory = studHistory.append(vars(item))
#     elif item.obj == "Instructor":
#         instHistory = instHistory.append(vars(item))
#     elif item.obj == "Aircraft":
#         aircraftHistory == aircraftHistory.append(vars(item))


######################################################################
# Constants                                                          #
######################################################################

RANDOM_SEED = 42
NUM_AIRCRAFT = 3
NUM_STUDENT = 1
NUM_INSTRUCTOR = 1

######################################################################
# Build Aircraft, Students, and                                      #
######################################################################


np.random.seed([RANDOM_SEED])


# def repair(env, ac)
env = simpy.Environment()
flightLine = []
buildAC(env, NUM_AIRCRAFT, flightLine)
studList = {0: Student(env, 0), 1: Student(env, 1), 2: Student(env, 2)}
# [Student(env, 1), Student(env, 2), Student(env, 3)]
instList = [Instructor(env, 11, 10),
            Instructor(env, 12, 10),
            Instructor(env, 13, 10)]
sked = Scheduler(env, flightLine, studList, instList)
env.run(until=50)

######################################################################
######################################################################

# Create an airplane to
# av1 = Avionics(env, "av1")
# af1 = Airframe(env, "af1")
# puls1 = Propulsion(env, "puls1")
# ac1 = Aircraft(env, af1, av1, puls1)
# av2 = Avionics(env, "av2")
# af2 = Airframe(env, "af2")
# puls2 = Propulsion(env, "puls2")
# ac2 = Aircraft(env, af2, av2, puls2)
# 
# availAC = [ac1, ac2]
# 
# stud1 = Student(env, 1)
# inst1 = Instructor(env, 11, 10)
# 
# 
# stud2 = Student(env, 2)
# inst2 = Instructor(env, 22, 20)
# 
# availStud = [stud1, stud2]
# 
# availInst = [inst1, inst2]
# 
# sked = Scheduler(env, availAC, availStud, availInst)


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

######################################################################
##                    Data Collection                               ##
######################################################################

partHistory = pd.DataFrame()
studHistory = pd.DataFrame()
instHistory = pd.DataFrame()
aicraftHistory = pd.DataFrame()

for stud in studList:
    studHistory = studHistory.append(studList[stud].flightDF)
    
